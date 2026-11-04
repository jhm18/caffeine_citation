#	fit_literatures.py
#	Nested degree-corrected stochastic blockmodel over the community network.
#
#	Jonathan H. Morgan and Sarah Delmar
#	03 November 2026
#
#	Run:
#		$GT_PYTHON python_env/fit_literatures.py \
#			--nodes  data/literatures/<spec_tag>/community_nodes.csv \
#			--edges  data/literatures/<spec_tag>/community_edges.csv \
#			--out    data/literatures/<spec_tag>
#
#	WHAT THIS FITS. Nodes are communities -- one per (era, community) pair, 3,557
#	of them under the primary specification. A directed edge from i to j carries
#	the number of citations from i's works to j's. S(literatures) asks for a
#	nested degree-corrected stochastic blockmodel with the number of groups
#	chosen by description length, and for a DISTRIBUTION over partitions rather
#	than a single one, because "a community sitting ambiguously between two
#	literatures is a substantively interesting object."
#
#	WHY DEGREE CORRECTION IS NOT OPTIONAL HERE. Community sizes run from 2 works
#	to 2,769, a ratio of 1,384 to 1. Without it the blocks recover size strata
#	and little else.
#
#	THE IDIOM IS THE VERIFIED ONE. graph-tool 3.9 takes base-state arguments
#	through base_state_args, NOT state_args -- an earlier script used the latter
#	and graph-tool answered "unrecognized keyword arguments", silently dropping
#	the argument. Every such call here runs with UserWarning promoted to an
#	exception, so a keyword this build does not know stops the fit instead of
#	producing a model that is quietly not the one asked for.
#
#	WEIGHTS ARE MULTIPLICITIES. Citation counts enter as eweight, which the
#	Poisson degree-corrected model reads as edge multiplicity -- ten citations
#	between two communities is ten parallel edges, which is what they are.

import argparse
import csv
import inspect
import os
import pickle
import sys
import time
import warnings

import numpy as np

try:
	import graph_tool.all as gt
except ModuleNotFoundError:
	GT_PYTHON = os.environ.get("GT_PYTHON", "/opt/gt-env/bin/python")
	sys.stderr.write(
		"\ngraph_tool is not importable from this interpreter.\n"
		"  running:  %s\n  need:     %s\n\nRun it as:\n    %s %s ...\n\n"
		% (sys.executable, GT_PYTHON, GT_PYTHON, os.path.basename(__file__)))
	raise SystemExit(1)


#######################
#   INTERNAL HELPERS   #
#######################

def read_table(path):
	"""Read a CSV into a list of dicts. csv rather than pandas: the graph-tool
	prefix is a bare conda environment and pandas is not a dependency of it."""
	with open(path, newline="") as handle:
		return list(csv.DictReader(handle))


def strict(call, *args, **kwargs):
	"""Run a graph-tool call with UserWarning promoted to an exception.

	This is the whole lesson of the state_args incident: a keyword graph-tool
	does not recognise is reported as a warning and then ignored, so the fit
	succeeds and is not the fit that was requested."""
	with warnings.catch_warnings():
		warnings.simplefilter("error", UserWarning)
		return call(*args, **kwargs)


def chain_levels(nested_blocks, n_vertices):
	"""
	Resolve each vertex's group at every level of the hierarchy.

	A nested partition is stored level by level: level 0 maps vertices to
	groups, level 1 maps level-0 GROUPS to groups, and so on. A vertex's
	level-2 literature is therefore level2[level1[level0[v]]], not level2[v].
	Reading the upper levels as though they were indexed by vertex is the
	mistake that produced a non-monotonic block count in the first probe.
	"""
	columns = []
	current = np.arange(n_vertices)
	for level in nested_blocks:
		level = np.asarray(level)
		current = level[current]
		columns.append(current.copy())
	return columns


##########################
#   BUILDING THE GRAPH   #
##########################

def build_graph(nodes, edges):
	"""Assemble the community network and its citation weights."""
	keys = [row["community_key"] for row in nodes]
	index = {key: position for position, key in enumerate(keys)}

	pairs = []
	weights = []
	dropped = 0
	for row in edges:
		source = index.get(row["community_i"])
		target = index.get(row["community_j"])
		if source is None or target is None:
			dropped += 1
			continue
		pairs.append((source, target))
		weights.append(int(float(row["weight"])))

	g = gt.Graph(directed=True)
	g.add_vertex(len(keys))
	g.add_edge_list(pairs)

	weight = g.new_edge_property("int32_t")
	weight.a = np.asarray(weights, dtype=np.int32)

	return g, weight, keys, dropped


#######################
#   FITTING THE SBM   #
#######################

def sampler_kwargs(niter):
	"""
	Use merge-split moves when this build offers them.

	Single-node MCMC mixes badly on a blockmodel: moving one community between
	literatures is nearly always rejected once the literatures are populated,
	so the chain crawls. Merge-split proposals move whole groups at once and
	are what makes equilibration tractable at this size. Checked rather than
	assumed, because an unrecognised keyword here would be silently dropped --
	the state_args lesson again.
	"""
	available = inspect.signature(gt.mcmc_equilibrate).parameters
	if "multiflip" in available:
		return dict(multiflip=True, mcmc_args=dict(niter=niter)), True
	return dict(mcmc_args=dict(niter=niter)), False


def progress(label, every, started):
	"""A callback that says the chain is alive and where its entropy is."""
	counter = {"n": 0, "best": None}

	def report(state):
		counter["n"] += 1
		entropy = state.entropy()
		if counter["best"] is None or entropy < counter["best"]:
			counter["best"] = entropy
		if counter["n"] % every == 0:
			print("    %s %6d   S = %12.2f   best = %12.2f   %6.1fs"
				  % (label, counter["n"], entropy, counter["best"],
					 time.time() - started), flush=True)
	return report


def fit(g, weight, draws, wait, niter, seed, max_iter, report_every):
	"""Minimize description length, then sample the posterior around it."""
	gt.seed_rng(seed)
	np.random.seed(seed)

	base_args = dict(deg_corr=True, eweight=weight)

	print("--- minimizing description length ---", flush=True)
	started = time.time()
	state = strict(gt.minimize_nested_blockmodel_dl, g,
				   base_state_args=base_args)
	levels = [level.get_B() for level in state.get_levels()]
	print("    groups per level:", levels)
	print("    description length:", round(state.entropy(), 3))
	print("    %.1fs" % (time.time() - started), flush=True)

	#	The Hierarchy Must Not Widen Going Up
	#	Every non-empty group at one level sits inside a group at the next, so
	#	the counts are non-increasing. A rise means the accessor is wrong or the
	#	state is not what it appears to be.
	if any(a < b for a, b in zip(levels, levels[1:])):
		raise SystemExit("fit_literatures: group counts rise up the hierarchy: "
						 "%s -- this is not a valid nested partition" % levels)

	#	The MDL Point Estimate Alone
	#	Enough for literature_i and literature_j in the primary fit, which is
	#	all S13.2 asks the design matrix for. The posterior is what the
	#	sensitivity analysis and the soft ARI need, and it costs orders of
	#	magnitude more, so it is opt-in rather than unavoidable.
	if draws <= 0:
		print("\n--- draws = 0, skipping the posterior ---", flush=True)
		return state, []

	sampler, multiflip = sampler_kwargs(niter)
	print("\n--- equilibrating (multiflip=%s, wait=%d, max_niter=%s) ---"
		  % (multiflip, wait, max_iter), flush=True)
	started = time.time()

	bounds = dict(wait=wait)
	if "max_niter" in inspect.signature(gt.mcmc_equilibrate).parameters:
		bounds["max_niter"] = max_iter
	else:
		print("    NOTE: this build has no max_niter; equilibration is "
			  "unbounded", flush=True)

	strict(gt.mcmc_equilibrate, state,
		   callback=progress("equilibrate", report_every, started),
		   **bounds, **sampler)
	print("    equilibrated in %.1fs" % (time.time() - started), flush=True)

	print("\n--- collecting %d draw(s) ---" % draws, flush=True)
	started = time.time()
	collected = []
	watch = progress("collect", report_every, started)

	def keep(s):
		collected.append(s.get_bs())
		watch(s)

	strict(gt.mcmc_equilibrate, state, force_niter=draws,
		   callback=keep, **sampler)
	print("    collected %d in %.1fs" % (len(collected),
										 time.time() - started), flush=True)
	return state, collected


def align(collected, out_dir, subsample=1):
	"""
	Put the draws on a common labelling.

	SAVED FIRST, ALIGNED SECOND, AND THE ORDER IS THE WHOLE POINT. Block labels
	are arbitrary in every draw -- the group holding one set of communities may
	be 7 in one sweep and 412 in the next -- so nothing can be averaged until
	they are reconciled. That reconciliation is iterative, its cost is not
	predictable from the number of draws, and in the first real run it took
	longer than the sampling that produced the draws. A run that then has to be
	killed would lose hours of MCMC to a post-processing step, so the draws go
	to disk before this is attempted and --from-draws brings them back.
	"""
	draws_path = os.path.join(out_dir, "posterior_draws.pkl")
	os.makedirs(out_dir, exist_ok=True)
	with open(draws_path, "wb") as handle:
		pickle.dump([[np.asarray(level) for level in nested]
					 for nested in collected], handle)
	print("    draws saved to", draws_path, "(%.1f MB)"
		  % (os.path.getsize(draws_path) / 1024.0 ** 2), flush=True)

	used = collected[::subsample] if subsample > 1 else collected
	if subsample > 1:
		print("    aligning every %dth draw: %d of %d"
			  % (subsample, len(used), len(collected)), flush=True)

	print("\n--- aligning draws (no progress output is available here) ---",
		  flush=True)
	started = time.time()
	mode = gt.PartitionModeState(used, nested=True, converge=True)
	print("    aligned in %.1fs" % (time.time() - started), flush=True)
	return mode



#######################
#   WRITING RESULTS   #
#######################

def write_assignments(path, keys, consensus, nodes):
	"""One row per community: its group at every level of the hierarchy."""
	columns = chain_levels(consensus, len(keys))
	headers = ["community_key", "era", "community"] + \
			  ["literature_l%d" % depth for depth in range(len(columns))]
	with open(path, "w", newline="") as handle:
		writer = csv.writer(handle)
		writer.writerow(headers)
		for position, key in enumerate(keys):
			row = [key, nodes[position]["era"], nodes[position]["community"]]
			row += [int(column[position]) for column in columns]
			writer.writerow(row)
	return len(columns)


def write_marginals(path, keys, marginal, g, n_draws):
	"""
	One row per community and literature: how often it landed there.

	Counts, not probabilities, are what get_marginal returns -- the entries sum
	to the number of draws. Both are written: the count so the denominator is
	visible, the probability because that is what the covariate lane wants.
	"""
	written = 0
	with open(path, "w", newline="") as handle:
		writer = csv.writer(handle)
		writer.writerow(["community_key", "literature", "n_draws", "prob"])
		for position, key in enumerate(keys):
			counts = np.asarray(marginal[g.vertex(position)], dtype=float)
			total = counts.sum()
			if total <= 0:
				continue
			for block, count in enumerate(counts):
				if count <= 0:
					continue
				writer.writerow([key, block, int(count),
								 round(float(count) / total, 6)])
				written += 1
	return written


def write_draws(path, keys, collected):
	"""Level-0 partition of every draw, for the sensitivity analysis."""
	with open(path, "w", newline="") as handle:
		writer = csv.writer(handle)
		writer.writerow(["draw", "community_key", "literature_l0"])
		for number, nested in enumerate(collected):
			level0 = np.asarray(nested[0])
			for position, key in enumerate(keys):
				writer.writerow([number, key, int(level0[position])])


##########################
#   EXECUTING THE FIT   #
##########################

if __name__ == "__main__":

	parser = argparse.ArgumentParser(
		description="Fit a nested degree-corrected SBM to the community network")
	parser.add_argument("--nodes", required=True)
	parser.add_argument("--edges", required=True)
	parser.add_argument("--out", required=True)
	parser.add_argument("--draws", type=int, default=2000,
						help="posterior draws to collect; 0 writes the MDL "
							 "point estimate alone and skips sampling "
							 "(default 2000)")
	parser.add_argument("--wait", type=int, default=200,
						help="iterations without a new entropy minimum before "
							 "equilibration stops (default 200)")
	parser.add_argument("--max-iter", type=int, default=5000,
						help="hard ceiling on equilibration iterations, so a "
							 "chain that will not settle still finishes "
							 "(default 5000)")
	parser.add_argument("--report-every", type=int, default=50,
						help="print entropy every N iterations (default 50)")
	parser.add_argument("--niter", type=int, default=10,
						help="sweeps per MCMC step (default 10)")
	parser.add_argument("--seed", type=int, default=20261103)
	parser.add_argument("--save-draws", action="store_true",
						help="also write every draw's level-0 partition as CSV")
	parser.add_argument("--from-draws", default=None,
						help="skip the MCMC and align a posterior_draws.pkl "
							 "saved by an earlier run")
	parser.add_argument("--align-subsample", type=int, default=1,
						help="align every Nth draw; alignment cost grows with "
							 "the number of draws and 2000 nested partitions "
							 "took longer than the sampling that made them "
							 "(default 1, all of them)")
	args = parser.parse_args()

	print("fit_literatures.py -- nested degree-corrected SBM")
	print("=================================================\n")
	print("graph-tool", gt.__version__)

	nodes = read_table(args.nodes)
	edges = read_table(args.edges)
	g, weight, keys, dropped = build_graph(nodes, edges)

	print("communities: %d" % g.num_vertices())
	print("edges:       %d distinct pair(s), %d citation(s)"
		  % (g.num_edges(), int(weight.a.sum())))
	if dropped:
		print("WARNING: %d edge row(s) referenced a community not in the node "
			  "table and were dropped" % dropped)
	print("weight range: %d to %d" % (int(weight.a.min()), int(weight.a.max())))
	print()

	if args.from_draws:
		#	Resume
		#	The expensive half is the MCMC and it is already on disk, so a
		#	slow or abandoned alignment costs nothing but its own time.
		print("\n--- loading draws from", args.from_draws, "---", flush=True)
		with open(args.from_draws, "rb") as handle:
			collected = pickle.load(handle)
		print("    %d draw(s)" % len(collected), flush=True)
		state = strict(gt.minimize_nested_blockmodel_dl, g,
					   base_state_args=dict(deg_corr=True, eweight=weight))
		mode = align(collected, args.out, args.align_subsample)
	else:
		state, collected = fit(g, weight, args.draws, args.wait,
							   args.niter, args.seed, args.max_iter,
							   args.report_every)
		mode = (align(collected, args.out, args.align_subsample)
				if collected else None)

	os.makedirs(args.out, exist_ok=True)

	#	With No Posterior, the Point Estimate Is the Answer
	#	get_bs() on the fitted state is the same nested structure the consensus
	#	would be read from, so the assignment file has the same shape either way
	#	and the covariate lane does not care which route produced it.
	if mode is None:
		consensus = state.get_bs()
		marginal = None
	else:
		consensus = mode.get_max_nested()
		marginal = mode.get_marginal(g)

	assignments_path = os.path.join(args.out, "community_literatures.csv")
	depth = write_assignments(assignments_path, keys, consensus, nodes)

	marginals_path = os.path.join(args.out, "literature_marginals.csv")
	rows = 0
	if marginal is not None:
		rows = write_marginals(marginals_path, keys, marginal, g,
							   len(collected))

	print("\n--- consensus ---")
	print("hierarchy depth:", depth)
	print("groups per level:",
		  [len(set(column.tolist()))
		   for column in chain_levels(consensus, len(keys))])

	#	How Much of the Grid Is Ambiguous
	#	The share of communities whose modal literature holds less than 90 per
	#	cent of their draws. S(literatures) calls these interesting rather than
	#	problematic, so the number is reported rather than hidden.
	if marginal is not None:
		ambiguous = 0
		for position in range(len(keys)):
			counts = np.asarray(marginal[g.vertex(position)], dtype=float)
			if counts.sum() > 0 and counts.max() / counts.sum() < 0.9:
				ambiguous += 1
		print("communities with a modal literature below 90%% of draws: "
			  "%d of %d (%.1f%%)"
			  % (ambiguous, len(keys), 100.0 * ambiguous / len(keys)))

	if args.save_draws:
		write_draws(os.path.join(args.out, "literature_draws.csv"),
					keys, collected)

	print("\nwrote:")
	print("  ", assignments_path)
	if marginal is None:
		print("   (no marginals: --draws 0, so there is no posterior to "
			  "summarise)")
	else:
		print("  ", marginals_path, "(%d row(s))" % rows)
	print("\nOK")
