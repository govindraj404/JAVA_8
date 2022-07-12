namespace com.sap.ic.cmh.businessObjectType;

using {com.sap.ic.cmh.streamType.StreamType} from './index';
type BusinessObjectType : Association to one BusinessObjectTypes;

entity BusinessObjectTypes {
    key code : String (40);
        name : localized String(60);
        streamType : StreamType;
        sequenceNumber : Integer;
}