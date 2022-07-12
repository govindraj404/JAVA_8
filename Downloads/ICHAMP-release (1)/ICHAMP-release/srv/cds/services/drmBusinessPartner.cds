using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';


service DrmBusinessPartnerService {

    entity DRMBusinessPartners as projection on cmh.businessPartner.BusinessPartners{
            key businessPartnerNumber as businessPartnerNumber,
            ID as businessPartnerId,
            businessPartnerName1 as businessPartnerName1,
            businessPartnerName2 as businessPartnerName2,
            businessPartnerType as businessPartnerType,
            addressID.email as email,
            addressID.mobile as mobile

   };

}