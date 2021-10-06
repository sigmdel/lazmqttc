
# Change Log

## 0.4.0 (current)

- Bumped up to version 0.4.0
- New screen shot 
- Added resource directory
- Added application options and option edit form


## 0.3.4 (0fd1b4f)

- Bumped up to version 0.3.4
- Updated translations
- If needed, automatically connects to a broker when the Publish button is pressed
- Published topic can optionaly be added to the Messages window 
- Added a temporary work directory in .hgignore


## 0.3.3 (94e0e89)

- Bumped up the version to 0.3.3
- Fixed errors in instructions about the `lazmqttc.desktop file`
- Updated the documentation including warning about lack of security 
- Added some missing translations in `lazmqtt.fr.po` and missing step in procedure `TBrokerEditForm.i18nFixup` (formally `i10nFixup`)
- Updated the PO files
- Added encryption of MQTT broker passwords in definition files


## 0.3.2 (b3e2325)

- Bumped up the version to 0.3.2
- Added missing .jr files
- Added feature to automatically clear a message after it is published
- Centered broker edit form on main form 


## 0.3.1 (9e48a6f, Release v0.3.1)

- Release of the version with NLS support
- Removed attempt to unsubscribe when not connected to the MQTT broker
- Cleaned up PO files


### 0.3.0 

- Added national language support and a translation into French
- Improved instructions on installation in a Linux system
- Updated documentation


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
