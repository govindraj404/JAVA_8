namespace com.sap.ic.cmh.complaintStatus;

type ComplaintStatus : Association to one ComplaintStatuses;

entity ComplaintStatuses {
    key code : String (40);
        name : localized String(60);
}