## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# buddhabrot <- lua_func("function(cfg)
#     local pool = luajr.workers()
# 
#     -- Per-worker setup: seed the RNG so each worker draws different samples.
#     -- srun runs this once in each worker, in sequence.
#     pool:srun(function(thread_id)
#         math.randomseed(thread_id * 7919)
#     end)
# 
#     -- One image buffer per worker, allocated in the main state. Each worker
#     -- writes only to its own buffer, so there's no race on shared pixels.
#     local bufs = {}
#     for t = 1, #pool do
#         bufs[t] = luajr.matrix(cfg.W, cfg.H, 0)
#     end
# 
#     -- Each worker samples `n_per` random points and accumulates the orbits
#     -- of those that escape into its own buffer.
#     local n_per = math.ceil(cfg.samples / #pool)
#     pool:prun(function(bufs, cfg, n_per, thread_id)
#         local img = bufs[thread_id]
#         local W, H, max_iter = cfg.W, cfg.H, cfg.max_iter
#         local cx, cy, scale = cfg.cx, cfg.cy, cfg.scale
#         local xr = scale * W / H   -- half-width in cr
# 
#         for s = 1, n_per do
#             local cr = cx + (math.random() * 2 - 1) * xr
#             local ci = cy + (math.random() * 2 - 1) * scale
#             local zr, zi, iter = 0, 0, 0
# 
#             -- First pass: does the orbit escape?
#             while zr*zr + zi*zi < 4 and iter < max_iter do
#                 zr, zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
#                 iter = iter + 1
#             end
# 
#             -- Only escaping orbits contribute (the Buddhabrot proper).
#             if iter < max_iter then
#                 local escape_iters = iter
#                 zr, zi = 0, 0
#                 for k = 1, escape_iters do
#                     zr, zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
#                     local i = math.floor((zr - cx) / xr * W / 2 + W / 2) + 1
#                     local j = math.floor((zi - cy) / scale * H / 2 + H / 2) + 1
#                     if i >= 1 and i <= W and j >= 1 and j <= H then
#                         img[(j - 1) * W + i] = img[(j - 1) * W + i] + 1
#                     end
#                 end
#             end
#         end
#     end, bufs, cfg, n_per)
# 
#     pool:close()
# 
#     -- Sum the per-worker buffers into one result.
#     local result = luajr.matrix(cfg.W, cfg.H, 0)
#     local n = cfg.W * cfg.H
#     for t = 1, #bufs do
#         local b = bufs[t]
#         for k = 1, n do
#             result[k] = result[k] + b[k]
#         end
#     end
#     return result
# end", "$.")
# 
# cfg <- list(W = 600, H = 400, max_iter = 200, samples = 1e8,
#             cx = -0.5, cy = 0, scale = 1.25)
# img <- buddhabrot(cfg)
# image(img, col = hcl.colors(256, "Oslo", rev = TRUE),
#       useRaster = TRUE, axes = FALSE)

## ----echo=FALSE, fig.align="center", out.width="600px"------------------------
knitr::include_graphics("images/buddhabrot.jpg")

