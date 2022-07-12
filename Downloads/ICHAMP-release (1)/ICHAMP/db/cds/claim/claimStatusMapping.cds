namespace com.sap.ic.cmh.claimStatusMapping;

using {com.sap.ic.cmh.claimStatus.ClaimStatus} from './index';

type ClaimStatusMapping : Association to one ClaimStatusMappings;

entity ClaimStatusMappings {
    key code : String (20);
        name : String(40);
        status : ClaimStatus;
}