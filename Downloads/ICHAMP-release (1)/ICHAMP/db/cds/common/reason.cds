namespace com.sap.ic.cmh.reason;

type Reason : Association to one Reasons;

entity Reasons {
    key code : String (10);
        description : localized String(40);
}