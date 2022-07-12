namespace com.sap.ic.cmh.streamType;

type StreamType : Association to one StreamTypes;

entity StreamTypes {
    key code : String (40);
        name : localized String(60);
}