using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from '../../../db/cds/customerComplaint/index';

service CustomerComplaintService {
    entity CustomerComplaints          as projection on cmh.customerComplaint.CustomerComplaints {
        *,
        @Core.Computed : false '' as referenceDocument     : String(10),
        @Core.Computed : false '' as referenceDocumentItem : String(10)
    };

    entity Attachments                 as projection on cmh.attachment.Attachments{
        *,
        @Core.Computed: false '' as readableFileSize : DataType.ReadableFileSize
    };
    
    @readonly
    entity SalesAreas                  as projection on cmh.salesArea.SalesAreas;

    @readonly
    entity ComplaintTypeConfigurations as projection on cmh.complaintTypeConfiguration.ComplaintTypeConfigurations;

    @readonly
    entity ItemCategories              as projection on cmh.itemCategory.ItemCategories;

    @readonly
    entity ComplaintChannels           as projection on cmh.complaintChannel.ComplaintChannels;

    @readonly
    entity ReturnFollowUpTypes         as projection on cmh.returnFollowUpType.ReturnFollowUpTypes;

};
