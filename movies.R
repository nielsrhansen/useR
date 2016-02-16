### Some norms ----
mynorm <- function(x, y, z, gamma = 1) abs(x)^gamma + abs(y)^gamma + abs(z)^gamma

groupnorm <- function(x, y, z) 
  sqrt(abs(x)^2 + abs(y)^2) + sqrt(abs(z)^2)

sparsegroupnorm <- function(x, y, z) 
  0.5 * (sqrt(abs(x)^2 + abs(y)^2) + sqrt(abs(z)^2)) + 
  0.5 * mynorm(x, y, z)

#### Lasso ball scene with point projected ----
xyz <- seq(-2, 2, 0.1)
val <- misc3d:::fgrid(mynorm, xyz, xyz, xyz)
x0 <- c(1.3, 0.8, 0.4)
z <- ((x0[1] - x0[2]) + 1) / 2
px0 <- c(z, 1 - z, 0)
contour3d(val, level = 1, color = "darkolivegreen3", light = c(0, 0, 4, 0.8), 
          shininess = 80, specular = "lightblue", 
          x = xyz, y = xyz, z = xyz)
rgl.viewpoint(theta = 50, phi = 20, zoom = 0.65)
lines3d(c(-1.5, 1.5), c(0, 0), c(0, 0))
lines3d(c(0, 0), c(-1.5, 1.5), c(0, 0))
lines3d(c(0, 0), c(0, 0), c(-1.5, 1.5))
spheres3d(x0[1], x0[2], x0[3], radius = 0.05, color = "dodgerblue2")
spheres3d(px0[1], px0[2], px0[3], radius = 0.05, color = "dodgerblue2")
lines3d(c(px0[1], x0[1]), c(px0[2], x0[2]), c(px0[3], x0[3]), color = "dodgerblue2", lwd = 3)


#### Lasso ball spinning ----
par3d(windowRect = 100 + 72 * c(0, 0, 7, 7))
movie3d(spin3d(axis = c(1, 1, 1), rpm = 10), duration = 6, 
        dir = getwd(), movie = "spinBall")

#### 3 lasso balls scene with point projected ----
x <- seq(0, 2, 0.1)
val <- misc3d:::fgrid(mynorm, x, xyz, xyz)
px1 <- c(0.5, 0, 0)
a <- (1.5 - x0[2] - x0[3] + 2 * x0[1]) / 3 
b <- (1.5 - x0[1] - x0[3] + 2 * x0[2]) / 3 
px2 <- c(a, b, 1.5 - a - b)
contour3d(val, level = c(0.5, 1, 1.5), color = c("darkolivegreen4", "darkolivegreen3", "darkolivegreen2"),
          alpha = 0.5, light = c(0, 0, 4, 0.8), 
          shininess = 80, specular = "lightblue", 
          x = x, y = xyz, z = xyz)
rgl.viewpoint(theta = 50, phi = 10, zoom = 0.65)
lines3d(c(-1.5, 1.5), c(0, 0), c(0, 0))
lines3d(c(0, 0), c(-1.5, 1.5), c(0, 0))
lines3d(c(0, 0), c(0, 0), c(-1.5, 1.5))
spheres3d(x0[1], x0[2], x0[3], radius = 0.05, color = "dodgerblue2")
spheres3d(px0[1], px0[2], px0[3], radius = 0.05, color = "dodgerblue2")
lines3d(c(px0[1], x0[1]), c(px0[2], x0[2]), c(px0[3], x0[3]), color = "dodgerblue2", lwd = 3)
spheres3d(px1[1], px1[2], px1[3], radius = 0.05, color = "dodgerblue2")
lines3d(c(px1[1], x0[1]), c(px1[2], x0[2]), c(px1[3], x0[3]), color = "dodgerblue2", lwd = 3)
spheres3d(px2[1], px2[2], px2[3], radius = 0.05, color = "dodgerblue2")
lines3d(c(px2[1], x0[1]), c(px2[2], x0[2]), c(px2[3], x0[3]), color = "dodgerblue2", lwd = 3)

#### 3 lasso balls spinning ----
par3d(windowRect = 100 + 72 * c(0, 0, 7, 7))
movie3d(spin3d(axis = c(1, 1, -1), rpm = 10), duration = 6, 
        dir = getwd(), movie = "cutSpinBall")


#### Lasso ball scene without point ----
xyz <- seq(-2, 2, 0.1)
val <- misc3d:::fgrid(mynorm, xyz, xyz, xyz)
x0 <- c(1.3, 0.8, 0.4)
z <- ((x0[1] - x0[2]) + 1) / 2
px0 <- c(z, 1 - z, 0)
contour3d(val, level = 1, color = "darkolivegreen3", light = c(0, 0, 4, 0.8), 
          shininess = 80, specular = "lightblue", 
          x = xyz, y = xyz, z = xyz)
rgl.viewpoint(theta = 50, phi = 20, zoom = 0.65)
lines3d(c(-1.5, 1.5), c(0, 0), c(0, 0))
lines3d(c(0, 0), c(-1.5, 1.5), c(0, 0))
lines3d(c(0, 0), c(0, 0), c(-1.5, 1.5))

#### Lasso ball spinning ----
par3d(windowRect = 100 + 72 * c(0, 0, 7, 7))
movie3d(spin3d(axis = c(1, 1, 1), rpm = 10), duration = 6, 
        dir = getwd(), movie = "spinBallone")


#### Group lasso ball scene without point ----
xyz <- seq(-2, 2, 0.01)
val <- misc3d:::fgrid(groupnorm, xyz, xyz, xyz)
contour3d(val, level = 1, color = "darkolivegreen3", light = c(0, 0, 4, 0.8), 
          shininess = 80, specular = "lightblue", 
          x = xyz, y = xyz, z = xyz)
rgl.viewpoint(theta = 50, phi = 20, zoom = 0.65)
lines3d(c(-1.5, 1.5), c(0, 0), c(0, 0))
lines3d(c(0, 0), c(-1.5, 1.5), c(0, 0))
lines3d(c(0, 0), c(0, 0), c(-1.5, 1.5))

#### Group lasso ball spinning ----
par3d(windowRect = 100 + 72 * c(0, 0, 7, 7))
movie3d(spin3d(axis = c(1, 1, 1), rpm = 10), duration = 6, 
        dir = getwd(), movie = "spinBallgroup")

#### Sparse group lasso ball scene without point ----

xyz <- seq(-2, 2, 0.01)
val <- misc3d:::fgrid(sparsegroupnorm, xyz, xyz, xyz)
contour3d(val, level = 1, color = "darkolivegreen3", light = c(0, 0, 4, 0.8), 
          shininess = 80, specular = "lightblue", 
          x = xyz, y = xyz, z = xyz)
rgl.viewpoint(theta = 50, phi = 20, zoom = 0.65)
lines3d(c(-1.5, 1.5), c(0, 0), c(0, 0))
lines3d(c(0, 0), c(-1.5, 1.5), c(0, 0))
lines3d(c(0, 0), c(0, 0), c(-1.5, 1.5))

#### Sparse group lasso ball spinning ----
par3d(windowRect = 100 + 72 * c(0, 0, 7, 7))
movie3d(spin3d(axis = c(1, 1, 1), rpm = 10), duration = 6, 
        dir = getwd(), movie = "spinBallsparsegroup")


