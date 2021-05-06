
# Change Log

<!-- ## Unreleased (development) -->


<!-- ## Released -->

### Current

- QoS for subscribed messages added
- QoS and Retain for published messages added
- All properties of `TMQTTConfig` record now included in `TBroker`
- Added a virtual grid (`TSubTopicsGrid` in `units/topicgrids.pas`) to make editing the `SubTopics` propety of `TBroker` simpler.
- `brokeredit.pas` rewritten to handle all the new properties of `TBroker`
- Rationalized editing of `TBroker` so that any changes made to the current MQTT broker definition in the main form are passed on to the broker editor form


### 0.1.0 (de68488)

- Should be last commit before adding QoS, Retain messages, etc.
- Improved TBroker.LoadFromFile
- Attribution for Unsubscribe methods in TMQTTConnection class added
- Added tests in TBroker unit test
- Added COPYING file
- Added this file

### 0.0.2  (0eda96b)

- Initial commit to GitHub.
