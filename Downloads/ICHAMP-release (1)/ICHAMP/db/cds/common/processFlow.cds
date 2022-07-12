namespace com.sap.ic.cmh.processFlow;

using {com.sap.ic.cmh.businessObjectRelation.BusinessObjectRelations} from './index';
using {com.sap.ic.cmh.combineBusinessObject.CommonBusinessObjects} from './index';

view ProcessFlow as select from BusinessObjectRelations join CommonBusinessObjects on 
    BusinessObjectRelations.sourceBusinessObjectUUID =  CommonBusinessObjects.ID 
    {
        key BusinessObjectRelations.ID          as ID,
        targetBusinessObjectUUID.complaint.ID    as complaintId,
        sourceBusinessObjectUUID,
        sourceBusinessObjectType.code as sourceBOTypeCode,
        sourceBusinessObjectType.name as sourceBOTypeName,
        sourceBusinessObjectType.streamType.code as sourceStreamTypeCode,
        sourceBusinessObjectType.streamType.name as sourceStreamTypeName,
        CommonBusinessObjects.identifier as sourceIdentifier,
        CommonBusinessObjects.createdAt as sourcecreatedAt,
        CommonBusinessObjects.createdBy as sourcecreatedBy,
        targetBusinessObjectUUID,
        targetBusinessObjectType.code as targetBOTypeCode ,
        targetBusinessObjectType.name as targetBOTypeName,
        targetBusinessObjectType.streamType.code as targetStreamTypeCode,
        targetBusinessObjectType.streamType.name as targetStreamTypeName,
        targetBusinessObjectUUID.identifier      as targetIdentifier,
        targetBusinessObjectUUID.createdAt as targetcreatedAt,
        targetBusinessObjectUUID.createdBy as targetcreatedBy,
        CommonBusinessObjects.ID as CommonBusinessObjectsID,
        CommonBusinessObjects.businessObjectStatuses as businessObjectStatuses
    };