(require 'underweb)

(underweb:start-server :port 5091)
(underweb:get! "/ping" ()
  `((ping . pong)))
