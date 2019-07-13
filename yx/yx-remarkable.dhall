let home = env:HOME as Text

in  { ssh =
        { host = "10.11.99.1", port = None Natural }
    , webUi =
        { host = "10.11.99.1", port = None Natural }
    , toolsPath =
        [ "${home}/.local/lib/yx-remarkable" ]
    , syncDir =
        "${home}/Documents/reMarkable"
    }
