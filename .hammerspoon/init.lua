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
