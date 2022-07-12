namespace com.sap.ic.cmh.streamStatus;

type StreamStatus : Association to one StreamStatuses;

entity StreamStatuses {
    key code : String (40);
        name : localized String(60);
}