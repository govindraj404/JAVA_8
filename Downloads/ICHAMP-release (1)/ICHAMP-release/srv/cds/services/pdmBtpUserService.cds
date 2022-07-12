using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';

using {com.sap.ic.cmh.complaint.dataType as datatype} from '../../../db/cds/complaint/index';

service PdmBtpUserService{
    entity BTPUsers {
         personResponsibleNumber : String;
        key personResponsible_ID : String
     };
    entity PDMComplaint   as
    select from cmh.complaint.Complaints{
        key ID,
        identifier as identifier,
        supplier.businessPartnerNumber as supplierNumber,
        supplier.businessPartnerName1 as supplierName,
        supplier.businessPartnerType as supplierType,
        contactPerson.businessPartnerNumber as contactPersonNumber,
        contactPerson.businessPartnerName1 as contactPersonName,
        contactPerson.businessPartnerType as contactPersontype,
        personResponsible_ID as personResponsible,
        createdBy,
        createdAt,
        modifiedAt,
        modifiedBy
    };
}