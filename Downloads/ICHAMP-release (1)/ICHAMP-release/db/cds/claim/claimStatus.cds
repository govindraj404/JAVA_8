namespace com.sap.ic.cmh.claimStatus;

type ClaimStatus : Association to one ClaimStatuses;

entity ClaimStatuses {
    key code : String (20);
        name : localized String(60);
        sequenceNumber : Integer;
}