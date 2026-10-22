# Repaired scenario review

Independent lean_expert STATIC APPROVE candidate31a42a69004c6282e01ad939c370f249fdf5e604, parentdd415d2cd70e39aae47099542041a672226a8133. No builds or mutations by reviewer.

Checked public camera construction is sound. Gate25 direct buffer constructor accesses were correctly refused. Gate28 retained6Pass2Fail/8JVM, noJS. Atlas correctly refused ephemeral grid; final uses public Grid.createPersistent, persistent catalog frame, explicit synthetic GridId; live owner and foreign refusals retained.

Linked centroid was numerically ambiguous: unpartitioned face2,0,1 yields1/3,1/3,1-1/3-1/3 and selectsvertex1 via exactlargest weight. Fix uses independently derived off-tie pixel56,72. Viewport uses129, pixelcenter56.5,72.5; worldxy=-32/129,-32/129; original vertexweights193/387,97/387,97/387, rounded RGB127,64,64, uniquevertex0. Cursor follows actualselectedvertex, retains-1,-1,0 -> voxel0,0,0 -> samevertex. NearestSample readback centroid uses partitioned geometry and staysunchanged.

No provider/API/tolerance/admission weakening. git diff --check passed. Full runtime gate31 still required.
