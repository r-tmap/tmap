data(World)

tm_shape(World[World$continent=="Africa",]) +
	tm_fill("pop_est")

tm_shape(World) +
	tm_fill("pop_est") +
	tm_layout_World()
