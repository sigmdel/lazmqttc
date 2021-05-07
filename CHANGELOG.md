
# Change Log

## 0.2.3 (e903cea, Release v0.2.3)

- Improved reporting when of publishing a message
- Added two forms to replace rather garish built-in Lazarus dialogs
- Removed the no longer needed `Delay` function in `main.pas`
- Made sure the `EditorDone` handler is called before processing `FormCloseQuery` in `brokeredit.pas` to avoid loosing changes made in an edit control that still had the focus
- Removed premature release v0.2.2


### 0.2.2 (bbb9e17)

- Ensured mosquitto does write log messages when running in Windows (there must be a better solution)
- Removed PairSplitter which would not play nice in Windows and replaced with a second splitter and an additional panel
- Fixed missing PayloadMemo handler in `brokeredit.pas`
- Added some documentation in `README.md` on use in Windows 10


### 0.2.1 (57488bd)

- Auto reconnect added

### 0.2.0 (f53e95e)
- QoS for subscribed messages added
- QoS and Retain for published messages added
- All properties of `TMQTTConfig` record now included in `TBroker`
- Added a virtual grid (`TSubTopicsGrid` in `units/topicgrids.pas`) to make editing the `SubTopics` propety of `TBroker` simpler
- `brokeredit.pas` rewritten to handle all the new properties of `TBroker`
- Rationalized editing of `TBroker` so that any changes made to the current MQTT broker definition in the main form are passed on to the broker editor form


## 0.1.0 (de68488, Pre-Release v0.1.0)

- Last commit before adding QoS, Retain messages, etc.
- Improved TBroker.LoadFromFile
- Attribution for Unsubscribe methods in TMQTTConnection class added
- Added tests in TBroker unit test
- Added COPYING file
- Added this file

### 0.0.2  (0eda96b)

- Initial commit to GitHub.
