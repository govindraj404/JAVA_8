namespace com.sap.ic.cmh.complaintType;

type ComplaintType : Association to one ComplaintTypes;

entity ComplaintTypes {
    key code : String (20);
        name : localized String(60);
}