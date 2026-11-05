#	level_entropy.py
#	What each level of the hierarchy costs, in description length.
#
#	Jonathan H. Morgan and Sarah Delmar
#	05 November 2026
#
#	Run:
#		$GT_PYTHON python_env/level_entropy.py \
#			--nodes  data/literatures/<spec>/community_nodes.csv \
#			--edges  data/literatures/<spec>/community_edges.csv \
#			--assignments data/literatures/<spec>_fast/community_literatures.csv \
#			--draws  data/literatures/<spec>_fast/posterior_draws.pkl \
#			--out    data/literatures/<spec>_fast
#
#	WHY THIS EXISTS. The nested hierarchy runs 1268, 405, 168, 82, 44, 25, 14,
#	8, 4, 2, 1, and the branching ratio is about 1.8 at every step. There is no
#	plateau and no gap, so the tree itself cannot say where a literature is. A
#	dendrogram drawn from it has no branch lengths -- its horizontal axis is
#	level number, not dissimilarity -- so reading a natural cut off the picture
#	is reading the layout rather than the data.
#
#	WHAT REPLACES THAT. Project each level's grouping back onto the communities,
#	hold it fixed, and ask what a flat degree-corrected blockmodel with that
#	grouping costs in description length. Coarsening always loses fit and always
#	saves complexity; the level where the loss starts to outrun the saving is
#	the granularity the data supports. That curve has a minimum. It is an
#	answer, not an impression.
#
#	NOTHING IS REFITTED. The partition comes off disk and is held fixed. This
#	measures the levels that were found, it does not search for better ones.
#
#	Output:
#		level_entropy.csv        one row per level of the consensus
#		level_entropy_draws.csv  the same curve per posterior draw, if supplied
#		level_entropy.png        the curve, with the posterior drawn over it

import argparse
import csv
import os
import pickle
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

try:
	import graph_tool.all as gt
except ModuleNotFoundError:
	GT_PYTHON = os.environ.get("GT_PYTHON", "/opt/gt-env/bin/python")
	sys.stderr.write(
		"\ngraph_tool is not importable from this interpreter.\n"
		"  running:  %s\n  need:     %s\n\nRun it as:\n    %s %s ...\n\n"
		% (sys.executable, GT_PYTHON, GT_PYTHON, os.path.basename(__file__)))
	raise SystemExit(1)

from fit_literatures import read_table, strict, build_graph


#######################
#   INTERNAL HELPERS   #
#######################

def contiguous(labels):
	"""Relabel to 0..B-1. graph-tool counts blocks by label range, so a
	partition labelled 0, 7, 412 is three groups described as though it had
	four hundred and thirteen."""
	_, index = np.unique(np.asarray(labels), return_inverse=True)
	return index.astype(np.int32)


def level_columns(rows):
	"""The literature_lN columns, in level order."""
	names = [k for k in rows[0].keys() if k.startswith("literature_l")]
	return sorted(names, key=lambda n: int(n.split("_l")[1]))


def project(rows, keys, column):
	"""That level's grouping, in graph vertex order."""
	by_key = {row["community_key"]: int(row[column]) for row in rows}
	missing = [k for k in keys if k not in by_key]
	if missing:
		raise SystemExit("level_entropy: %d community(s) absent from the "
						 "assignments, first %s" % (len(missing), missing[0]))
	return contiguous([by_key[k] for k in keys])


def project_draw(nested, n_vertices):
	"""
	Every level of one draw, projected onto vertices.

	A draw is stored level by level: level 0 maps vertices to groups, level 1
	maps level-0 GROUPS to groups. A vertex's level-2 group is therefore
	level2[level1[level0[v]]], and reading upper levels as vertex-indexed is
	how the first probe produced an impossible block count.
	"""
	out = []
	current = np.arange(n_vertices)
	for level in nested:
		current = np.asarray(level)[current]
		out.append(contiguous(current))
	return out


def flat_cost(g, weight, labels):
	"""Description length of a flat degree-corrected blockmodel at this
	grouping, held fixed."""
	state = strict(gt.BlockState, g, b=labels, deg_corr=True, eweight=weight)
	return float(state.entropy()), int(len(np.unique(labels)))


######################
#   DRAWING THE CURVE   #
######################

def draw_curve(results, draw_rows, path):
	"""
	Description length against number of groups, with the posterior over it.

	The x axis is group count on a log scale and runs fine-to-coarse, because
	the question the picture answers is how much is lost by merging. Vertical
	distance is the only thing that carries meaning: this is the branch length
	the dendrogram of the hierarchy does not have, its own x axis being level
	number rather than any dissimilarity.
	"""
	try:
		import matplotlib
		matplotlib.use("Agg")
		import matplotlib.pyplot as plt
	except ImportError:
		print("matplotlib not available in this prefix; skipping the plot")
		return None

	plt.rcParams.update({"font.family": "serif", "font.size": 11})
	fig, ax = plt.subplots(figsize=(10, 6.6), dpi=150)
	ink, accent = "#3a3a38", "#8c2f1d"

	#	Every draw, faintly
	if draw_rows:
		by_draw = {}
		for row in draw_rows:
			by_draw.setdefault(row[0], []).append((row[2], row[3]))
		for series in by_draw.values():
			series = [(gg, dd) for gg, dd in series if gg > 1]
			series.sort(key=lambda t: -t[0])
			ax.plot([t[0] for t in series], [t[1] for t in series],
					color=ink, alpha=0.08, linewidth=1, zorder=1)

	points = [r for r in results if r["n_groups"] > 1]
	xs = [r["n_groups"] for r in points]
	ys = [r["description_length"] for r in points]
	ax.plot(xs, ys, color=ink, linewidth=2.4, zorder=3)
	ax.scatter(xs, ys, s=58, facecolor="white", edgecolor=ink,
			   linewidth=1.6, zorder=4)

	#	The best fit, and the last merge that is nearly free
	best = min(points, key=lambda r: r["description_length"])
	floor = best["description_length"]
	cheap = [r for r in points
			 if r["n_groups"] < best["n_groups"]
			 and (r["description_length"] - floor) / floor < 0.05]
	elbow = min(cheap, key=lambda r: r["n_groups"]) if cheap else best

	for r, label in ((best, "best fit"), (elbow, "cheapest usable merge")):
		ax.scatter([r["n_groups"]], [r["description_length"]], s=120,
				   facecolor=accent, edgecolor=ink, linewidth=1.6, zorder=5)
		ax.annotate("%d\n%s" % (r["n_groups"], label),
					(r["n_groups"], r["description_length"]),
					textcoords="offset points", xytext=(0, -42),
					ha="center", color=accent, fontsize=10)
	if elbow is not best:
		ax.axvline(elbow["n_groups"], color=accent, linestyle="--",
				   linewidth=1.3, zorder=2)

	#	Price every coarser level against the best
	for r in points:
		if r["n_groups"] >= elbow["n_groups"]:
			continue
		excess = 100.0 * (r["description_length"] - floor) / floor
		if excess < 40:
			continue
		ax.annotate("%d\n+%.0f%%" % (r["n_groups"], excess),
					(r["n_groups"], r["description_length"]),
					textcoords="offset points", xytext=(0, -40),
					ha="center", color=ink, fontsize=9)

	ax.set_xscale("log")
	ax.invert_xaxis()
	ax.set_xticks(xs)
	ax.set_xticklabels([str(v) for v in xs])
	ax.minorticks_off()
	ax.set_xlabel("groups in the flat partition")
	ax.set_ylabel("description length, nats")
	ax.set_title("What each level of the hierarchy costs to describe",
				 fontsize=14, pad=18)
	note = ("flat degree-corrected SBM at each level's grouping, held fixed"
			+ ("；  faint lines are %d posterior draws" % len(by_draw)
			   if draw_rows else ""))
	ax.annotate(note.replace("；", ";"), xy=(0.5, 1.015),
				xycoords="axes fraction", ha="center", fontsize=9,
				color="#6b6a66")
	for side in ("top", "right"):
		ax.spines[side].set_visible(False)
	ax.margins(y=0.14)
	fig.tight_layout()
	fig.savefig(path)
	plt.close(fig)
	return path


##########################
#   RUNNING THE CURVE   #
##########################

if __name__ == "__main__":

	parser = argparse.ArgumentParser(
		description="Description length at each level of the hierarchy")
	parser.add_argument("--nodes", required=True)
	parser.add_argument("--edges", required=True)
	parser.add_argument("--assignments", required=True)
	parser.add_argument("--draws", default=None,
						help="posterior_draws.pkl, for uncertainty on the curve")
	parser.add_argument("--draw-sample", type=int, default=50,
						help="draws to evaluate (default 50)")
	parser.add_argument("--out", required=True)
	parser.add_argument("--no-plot", action="store_true",
						help="skip the figure")
	args = parser.parse_args()

	print("level_entropy.py -- what each level costs")
	print("=========================================\n")
	print("graph-tool", gt.__version__)

	nodes = read_table(args.nodes)
	edges = read_table(args.edges)
	g, weight, keys, dropped = build_graph(nodes, edges)
	print("communities: %d   pairs: %d   citations: %d"
		  % (g.num_vertices(), g.num_edges(), int(weight.a.sum())))
	if dropped:
		print("WARNING: %d edge row(s) dropped" % dropped)

	rows = read_table(args.assignments)
	columns = level_columns(rows)
	print("levels in the assignments: %d\n" % len(columns))

	#	The Consensus Curve
	results = []
	for depth, column in enumerate(columns):
		labels = project(rows, keys, column)
		dl, n_groups = flat_cost(g, weight, labels)
		results.append(dict(level=depth, n_groups=n_groups, description_length=dl))
		print("    level %2d  %5d group(s)   DL = %14.2f"
			  % (depth, n_groups, dl), flush=True)

	best = min(results, key=lambda r: r["description_length"])
	for r in results:
		r["excess_over_best"] = round(
			r["description_length"] - best["description_length"], 2)
		r["description_length"] = round(r["description_length"], 2)

	#	The Cost of Each Merge
	#	Going from one level to the next coarser one. Positive means coarsening
	#	cost description length; negative means it paid.
	for i in range(len(results) - 1):
		results[i + 1]["merge_cost"] = round(
			results[i + 1]["description_length"] -
			results[i]["description_length"], 2)
	results[0]["merge_cost"] = ""

	os.makedirs(args.out, exist_ok=True)
	path = os.path.join(args.out, "level_entropy.csv")
	with open(path, "w", newline="") as handle:
		writer = csv.DictWriter(handle, fieldnames=[
			"level", "n_groups", "description_length", "excess_over_best",
			"merge_cost"])
		writer.writeheader()
		writer.writerows(results)

	print("\nminimum description length at level %d, %d group(s)"
		  % (best["level"], best["n_groups"]))
	print("wrote", path)

	#	The Same Curve, Across Posterior Draws
	#	Each draw carries its own hierarchy, so levels are not comparable
	#	between draws -- but the number of groups is. Plotting DL against group
	#	count puts every draw on one axis and shows whether the consensus
	#	minimum sits where the posterior agrees it should.
	draw_rows = []
	if args.draws:
		print("\n--- posterior spread ---", flush=True)
		with open(args.draws, "rb") as handle:
			collected = pickle.load(handle)
		step = max(1, len(collected) // args.draw_sample)
		used = collected[::step][:args.draw_sample]
		print("evaluating %d of %d draw(s)" % (len(used), len(collected)),
			  flush=True)

		spread_path = os.path.join(args.out, "level_entropy_draws.csv")
		with open(spread_path, "w", newline="") as handle:
			writer = csv.writer(handle)
			writer.writerow(["draw", "level", "n_groups", "description_length"])
			for number, nested in enumerate(used):
				for depth, labels in enumerate(
						project_draw(nested, g.num_vertices())):
					dl, n_groups = flat_cost(g, weight, labels)
					writer.writerow([number, depth, n_groups, round(dl, 2)])
					draw_rows.append((number, depth, n_groups, dl))
				if (number + 1) % 10 == 0:
					print("    %d draw(s) done" % (number + 1), flush=True)
		print("wrote", spread_path)

	#	Draw
	if not args.no_plot:
		drawn = draw_curve(results, draw_rows,
						   os.path.join(args.out, "level_entropy.png"))
		if drawn:
			print("wrote", drawn)

	print("\nOK")
