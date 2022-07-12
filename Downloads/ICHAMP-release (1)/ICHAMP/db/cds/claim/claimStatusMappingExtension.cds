namespace com.sap.ic.cmh.claimStatusMappingExtension;

using {com.sap.ic.cmh.claimStatus.ClaimStatus} from './index';
using {cuid, managed} from '@sap/cds/common';

type ClaimStatusMappingExtension : Association to one ClaimStatusMappingExtensions;

@cds.search: { code, name, status}

entity ClaimStatusMappingExtensions : cuid, managed {
    identifier : Integer;
    code : String (20);
    name : String(40);
    status : ClaimStatus;
}