#!/usr/bin/env bash
#	install_graph_tool.sh
#	Install graph-tool into the running container, in its own environment.
#
#	Run inside the container:
#		bash install_graph_tool.sh
#
#	Why conda and not apt or pip. graph-tool is not pip-installable. The author's
#	apt repository no longer serves jammy -- it now lists Debian sid/forky and
#	Ubuntu questing/resolute -- and a source build needs GCC 15+ and Boost 1.80+,
#	against the GCC 11 and Boost 1.74 that 22.04 ships. conda-forge is upstream's
#	own first recommendation and the only route that does not touch the base.
#
#	Why its own prefix. PATH already starts with /opt/sbi-env/bin, and
#	RETICULATE_PYTHON and PyCall both point at /usr/bin/python3. This adds a third
#	interpreter at /opt/gt-env and puts nothing on PATH, so neither of those
#	moves. Call it by absolute path.
#
#	NOTE ON PERSISTENCE: this writes to /opt inside the container. It survives
#	restarts of this container and does NOT survive a rebuild. To keep it, either
#	commit the container or move section 21 into the image later.

set -euo pipefail

CONDA_PREFIX_DIR="${CONDA_PREFIX_DIR:-/opt/conda}"
GT_PREFIX="${GT_PREFIX:-/opt/gt-env}"
MINIFORGE_URL="https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh"

echo "=== graph-tool install ==="
echo "miniforge prefix: ${CONDA_PREFIX_DIR}"
echo "graph-tool env:   ${GT_PREFIX}"
echo

#	Already there?
if [ -x "${GT_PREFIX}/bin/python" ] && \
   "${GT_PREFIX}/bin/python" -c "import graph_tool" 2>/dev/null; then
	echo "graph-tool is already installed at ${GT_PREFIX}"
	"${GT_PREFIX}/bin/python" -c \
		"import graph_tool.all as gt; print('version:', gt.__version__)"
	exit 0
fi

#	Miniforge
if [ ! -x "${CONDA_PREFIX_DIR}/bin/conda" ]; then
	echo "--- installing miniforge ---"
	wget -q --show-progress -O /tmp/miniforge.sh "${MINIFORGE_URL}"
	bash /tmp/miniforge.sh -b -p "${CONDA_PREFIX_DIR}"
	rm -f /tmp/miniforge.sh
else
	echo "--- miniforge already present, reusing ---"
fi

#	The Environment
#	A fresh prefix, not an install into an existing env. Upstream warns the
#	solver is likely to fail or hang when the target environment already holds
#	many packages, and this image holds a great many.
echo
echo "--- solving and installing graph-tool (several minutes) ---"
"${CONDA_PREFIX_DIR}/bin/conda" create -y -p "${GT_PREFIX}" -c conda-forge \
	graph-tool

#	Trim
"${CONDA_PREFIX_DIR}/bin/conda" clean -afy

#	Verify, Loudly
#	An import that fails here is better than one that fails three hours into a
#	fit, so the script exits nonzero rather than reporting success.
echo
echo "--- verifying ---"
"${GT_PREFIX}/bin/python" - <<'PY'
import graph_tool.all as gt
print("graph-tool", gt.__version__)

#	The two things the lane actually needs: a nested degree-corrected fit and
#	the ability to sample partitions from the posterior.
g = gt.collection.data["karate"]
state = gt.NestedBlockState(g, state_args=dict(deg_corr=True))
gt.mcmc_equilibrate(state, wait=10, force_niter=20, mcmc_args=dict(niter=5))
levels = [len(set(s.get_blocks().a)) for s in state.get_levels()]
print("nested DC-SBM OK; blocks per level:", levels)
print("description length:", round(state.entropy(), 2))
PY

echo
echo "=== done ==="
echo "Interpreter: ${GT_PREFIX}/bin/python"
echo
echo "Add to ~/.bashrc and ~/.Renviron so the lane can find it without a"
echo "hard-coded path, but do NOT put ${GT_PREFIX}/bin on PATH:"
echo
echo "    echo 'export GT_PYTHON=${GT_PREFIX}/bin/python' >> /root/.bashrc"
echo "    echo 'GT_PYTHON=${GT_PREFIX}/bin/python' >> /root/.Renviron"
