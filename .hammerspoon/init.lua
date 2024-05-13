function activateFn(hint)
  return function()
    local app = hs.application.find(hint)
    app:activate()
  end
end

hs.hotkey.bind({ "alt" }, "1", activateFn("emacs"))
hs.hotkey.bind({ "alt" }, "2", activateFn("safari"))
hs.hotkey.bind({ "alt" }, "3", activateFn("slack"))
hs.hotkey.bind({ "alt" }, "4", activateFn("kitty"))

hs.hotkey.bind({ "alt" }, "5", function()
  local zoom = hs.application.find("zoom")
  local win = zoom:findWindow("Zoom Meeting")
  win:focus()
end)

hs.hotkey.bind({ "cmd", "shift" }, "v", function()
  hs.eventtap.keyStrokes(hs.pasteboard.getContents())
end)

local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0
spoon.MiroWindowsManager:bindHotkeys({
  up = {hyper, "up"},
  right = {hyper, "right"},
  down = {hyper, "down"},
  left = {hyper, "left"},
  fullscreen = {hyper, "f"},
  nextscreen = {hyper, "n"}
})

hs.loadSpoon("URLDispatcher")

spoon.URLDispatcher.default_handler = "org.qutebrowser.qutebrowser"
spoon.URLDispatcher.url_patterns = {
   -- Youtube works better in Chrome
   { "https?://www.youtube.com", "com.google.chrome" },
   -- Work URLs which require Okta (so Chrome)
   { "https?://sourcegraph.slack.com", "com.google.chrome" },
   { "https?://docs.google.com", "com.google.chrome" },
   { "https?://console.cloud.google.com", "com.google.chrome" },
   { "https?://ui.honeycomb.io", "com.google.chrome" },
   { "https?://figma.com", "com.google.chrome" },
   { "https?://linear.app", "com.google.chrome" },
   { "https?://notion.so", "com.google.chrome" }
}
--spoon.URLDispatcher.logger.setLogLevel("debug")
spoon.URLDispatcher:start()
