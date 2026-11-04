#	verify_graph_tool.py
#	Establish the exact nested DC-SBM idiom this build of graph-tool accepts.
#
#	Jonathan H. Morgan and Sarah Delmar
#	03 November 2026
#
#	Run:
#		$GT_PYTHON python_env/verify_graph_tool.py
#
#	WHY THIS EXISTS. The first install check called
#
#		gt.NestedBlockState(g, state_args=dict(deg_corr=True))
#
#	and graph-tool answered "unrecognized keyword arguments: ['state_args']".
#	That is not a cosmetic warning. The keyword was DROPPED, so deg_corr never
#	reached the base state and the fit that printed "OK" may not have been
#	degree-corrected at all -- the one property S(literatures) says the method
#	must have, because community sizes here run from 2 works to 2,769.
#
#	The 3.x documentation is not explicit about which keyword replaced it or what
#	deg_corr defaults to, so nothing here is taken on trust. The script reads the
#	installed signatures, picks the keyword this build actually declares, and
#	turns UserWarning into an exception so that a silently ignored argument stops
#	the run instead of passing it.

import inspect
import os
import sys
import warnings

import numpy as np

#	The Right Interpreter
#	graph-tool lives in its own conda prefix and is deliberately NOT on PATH,
#	because PATH starts with /opt/sbi-env/bin and RETICULATE_PYTHON and PyCall
#	both point at /usr/bin/python3. So a bare `python` reaches an interpreter
#	that has never heard of graph_tool, and the ModuleNotFoundError that follows
#	says nothing about which one to use. This says it.
try:
	import graph_tool.all as gt
except ModuleNotFoundError:
	GT_PYTHON = os.environ.get("GT_PYTHON", "/opt/gt-env/bin/python")
	sys.stderr.write(
		"\ngraph_tool is not importable from this interpreter.\n"
		"  running:  %s\n"
		"  need:     %s\n\n"
		"Run it as:\n"
		"    %s %s\n\n"
		"graph-tool is kept off PATH on purpose: /opt/sbi-env/bin comes first\n"
		"and RETICULATE_PYTHON and PyCall both point at /usr/bin/python3, so\n"
		"putting it on PATH would shadow the venv for every bare python call.\n"
		"If %s does not exist, run scripts/install_graph_tool.sh first.\n\n"
		% (sys.executable, GT_PYTHON, GT_PYTHON,
		   os.path.basename(__file__), GT_PYTHON))
	raise SystemExit(1)


#######################
#   WHAT THIS BUILD   #
#######################

def report_signatures():
	"""Print the constructor signatures that decide how deg_corr is passed."""
	print("graph-tool", gt.__version__)
	print()

	nested = inspect.signature(gt.NestedBlockState.__init__)
	print("NestedBlockState.__init__:")
	for name, param in nested.parameters.items():
		if name == "self":
			continue
		print("    %-20s %s" % (name, param.default
								if param.default is not param.empty else "<required>"))

	base = inspect.signature(gt.BlockState.__init__)
	print("\nBlockState.__init__ (deg_corr lives here):")
	deg = base.parameters.get("deg_corr")
	print("    deg_corr default:",
		  deg.default if deg is not None else "NOT A NAMED PARAMETER")
	return nested


def degree_correction_kwargs(nested):
	"""Pick the keyword THIS build declares for base-state arguments."""
	if "base_state_args" in nested.parameters:
		return dict(base_state_args=dict(deg_corr=True)), "base_state_args"
	if "state_args" in nested.parameters:
		return dict(state_args=dict(deg_corr=True)), "state_args"
	return dict(deg_corr=True), "direct kwarg"


##################
#   THE LEVELS   #
##################

def blocks_per_level(state):
	"""
	Count groups per level, by every accessor this build offers.

	The naive len(set(sl.get_blocks().a)) gave [1, 3, 1, 2, 1, 1, 1], which
	cannot be a hierarchy: every non-empty group at level l+1 contains at least
	one non-empty group from level l, so the counts are non-increasing as you go
	up. A sequence that rises is measuring label values rather than occupied
	groups. Both are printed so the disagreement is visible rather than assumed.
	"""
	levels = state.get_levels()
	rows = []
	for depth, sl in enumerate(levels):
		row = {"level": depth}
		row["get_B"] = sl.get_B() if hasattr(sl, "get_B") else None
		row["get_nonempty_B"] = (sl.get_nonempty_B()
								 if hasattr(sl, "get_nonempty_B") else None)
		row["naive_unique"] = len(set(np.asarray(sl.get_blocks().a)))
		rows.append(row)
	return rows


def monotone(values):
	"""A hierarchy's group counts never increase going up."""
	clean = [v for v in values if v is not None]
	return all(a >= b for a, b in zip(clean, clean[1:]))


def adjusted_rand(a, b):
	"""
	Adjusted Rand index between two labelings, in numpy.

	Hand-written rather than taken from graph_tool or sklearn: sklearn lives in
	the sbi venv and not in this prefix, and the point of the check it serves is
	to test graph-tool's output against something graph-tool did not compute.
	This is also the statistic S(literatures) names for the journal validation,
	so it will be wanted again.
	"""
	a = np.asarray(a)
	b = np.asarray(b)
	_, ia = np.unique(a, return_inverse=True)
	_, ib = np.unique(b, return_inverse=True)
	table = np.zeros((ia.max() + 1, ib.max() + 1), dtype=np.int64)
	np.add.at(table, (ia, ib), 1)

	def pairs(x):
		return (x * (x - 1) / 2).sum()

	cells = pairs(table)
	rows = pairs(table.sum(axis=1))
	cols = pairs(table.sum(axis=0))
	total = table.sum() * (table.sum() - 1) / 2
	expected = rows * cols / total
	ceiling = (rows + cols) / 2
	return (cells - expected) / (ceiling - expected)


##########################
#   RUNNING THE CHECKS   #
##########################

if __name__ == "__main__":

	nested = report_signatures()
	kwargs, which = degree_correction_kwargs(nested)
	print("\nPassing deg_corr via:", which)

	g = gt.collection.data["karate"]
	print("fixture: karate, %d vertices, %d edges\n"
		  % (g.num_vertices(), g.num_edges()))

	#	A Dropped Keyword Is an Error, Not a Warning
	#	This is the whole point of the script. If this build does not recognise
	#	the keyword, the run stops here rather than reporting a fit whose degree
	#	correction went nowhere.
	with warnings.catch_warnings():
		warnings.simplefilter("error", UserWarning)
		state = gt.minimize_nested_blockmodel_dl(g, **kwargs)
	print("minimize_nested_blockmodel_dl accepted", which, "with no warning")

	#	Confirm It Took
	#	Reading the flag off the fitted base state, rather than trusting that
	#	passing the argument meant anything.
	base = state.get_levels()[0]
	print("level 0 deg_corr =",
		  getattr(base, "deg_corr", "ATTRIBUTE ABSENT"))

	#	The Hierarchy
	print("\ngroups per level:")
	rows = blocks_per_level(state)
	for row in rows:
		print("    level %d: get_B=%s  get_nonempty_B=%s  naive_unique=%s"
			  % (row["level"], row["get_B"], row["get_nonempty_B"],
				 row["naive_unique"]))
	for key in ("get_B", "get_nonempty_B", "naive_unique"):
		series = [r[key] for r in rows]
		print("    %-16s monotone non-increasing: %s"
			  % (key, monotone(series)))

	print("\ndescription length (entropy):", round(state.entropy(), 3))

	#	Posterior Draws and Soft Membership
	#	Short run -- this is a smoke test of the idiom, not a fit. The lane uses
	#	the same four calls with a real wait and force_niter.
	print("\nsampling the posterior (short run)...")
	gt.mcmc_equilibrate(state, wait=100, mcmc_args=dict(niter=5))

	draws = []
	gt.mcmc_equilibrate(state, force_niter=200, mcmc_args=dict(niter=5),
						callback=lambda s: draws.append(s.get_bs()))
	print("collected", len(draws), "nested partition(s)")

	pmode = gt.PartitionModeState(draws, nested=True, converge=True)
	marginal = pmode.get_marginal(g)
	consensus = pmode.get_max_nested()
	print("consensus hierarchy depth:", len(consensus))
	print("consensus groups per level:",
		  [len(set(np.asarray(level))) for level in consensus])

	#	What the Marginal Looks Like
	#	A vector<int32_t> of COUNTS per block, not probabilities: the entries sum
	#	to the number of draws. The lane divides by that to get the probability
	#	per community per literature.
	first = g.vertex(0)
	counts = np.asarray(marginal[first], dtype=float)
	print("\nmarginal for vertex 0 (counts):", list(counts.astype(int)))
	print("  sums to", int(counts.sum()), "against", len(draws), "draw(s)")
	print("  as probabilities:", list(np.round(counts / counts.sum(), 4)))
	print("marginal property type:", marginal.value_type())

	#	Does It Recover a Partition We Planted?
	#	Karate collapsing to one block is probably MDL being honest -- 34 nodes
	#	and 78 edges is thin evidence, and the marginal above shows the sampler
	#	moving between one and two blocks rather than sitting still. But a method
	#	that finds nothing where there is little to find tells us nothing about
	#	whether it finds what IS there. So: four blocks, planted, and a graph
	#	nearer the size of the community network the lane will actually fit.
	print("\n--- planted partition recovery ---")
	rng = np.random.default_rng(20261103)
	per_block, n_blocks = 75, 4
	n = per_block * n_blocks
	planted = np.repeat(np.arange(n_blocks), per_block)
	p_in, p_out = 0.08, 0.004

	upper = np.triu_indices(n, 1)
	same = planted[upper[0]] == planted[upper[1]]
	draw = rng.random(same.size) < np.where(same, p_in, p_out)
	edges = np.column_stack((upper[0][draw], upper[1][draw]))

	planted_graph = gt.Graph(directed=False)
	planted_graph.add_edge_list(edges)
	print("planted: %d vertices, %d edges, %d blocks of %d"
		  % (planted_graph.num_vertices(), planted_graph.num_edges(),
			 n_blocks, per_block))

	with warnings.catch_warnings():
		warnings.simplefilter("error", UserWarning)
		fit = gt.minimize_nested_blockmodel_dl(planted_graph, **kwargs)

	recovered = np.asarray(fit.get_levels()[0].get_blocks().a)[:n]
	print("recovered groups per level:",
		  [sl.get_B() for sl in fit.get_levels()])
	print("description length:", round(fit.entropy(), 3))
	print("adjusted Rand against the planted partition:",
		  round(adjusted_rand(planted, recovered), 4))

	print("\nOK")
