using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.common.dataType as DataType} from '../../../db/cds/common/index';
using {com.sap.ic.cmh.complaint.dataType as Dt} from '../../../db/cds/complaint/index';

service ManageClaimService {
    @Capabilities.Deletable : false
    entity Claims as projection on cmh.claim.Claims{
     *,
    @Core.Computed: false '' as contactPersonCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as complaintCode : Dt.Identifier,
    @Core.Computed: false '' as personResponsibleCode : DataType.BusinessPartnerNumber
    };
}