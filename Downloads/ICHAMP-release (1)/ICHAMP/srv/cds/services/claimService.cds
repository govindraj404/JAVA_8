using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.claim.dataType as datatype} from '../../../db/cds/claim/index';

service ClaimService {
    entity Claims as projection on cmh.claim.Claims{
        *,
    @Core.Computed: false 3 as isClaimFieldControl : datatype.FieldControl,
    @Core.Computed: false false as isUpdateRestricted : datatype.UpdateRestricted,
    @Core.Computed: false 3 as sNavigation : String(20),
    '' as number : datatype.Identifier
    };
}
