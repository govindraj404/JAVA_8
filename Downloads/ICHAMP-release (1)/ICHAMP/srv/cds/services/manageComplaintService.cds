using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.common.dataType as DataType} from '../../../db/cds/common/index';
using {com.sap.ic.cmh.complaint.dataType as datatype} from '../../../db/cds/complaint/index';

service ManageComplaintService {
  @Capabilities.Deletable : false
  entity Complaints                    as projection on cmh.complaint.Complaints{
    *,
    @Core.Computed: false '' as supplierCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as materialCode : DataType.MaterialCode,
    @Core.Computed: false '' as plantCode : DataType.Plant,
    @Core.Computed: false '' as purchaseOrganisationCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as contactPersonCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as personResponsibleCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false false as isEventSpecificRequest  : Boolean
  };
  
  entity BusinessObjects               as projection on cmh.businessObject.BusinessObjects;

  entity Streams                       as projection on cmh.stream.Streams;

  @Capabilities.Updatable : false
  entity CostCollectors as projection on cmh.costCollector.CostCollectors{
   *,
   @Core.Computed: false '' as complaintCode : datatype.Identifier,
  };

}
