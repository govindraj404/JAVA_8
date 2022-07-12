package com.sap.ic.cmh.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import cds.gen.manageclaimservice.Claims;
import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managesupplierissueprocessservice.Supplier8DProcesses;
import cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.managecomplaintservice.CostCollectors;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.claim.model.binary_relation.DocumentFlow;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.gen.MessageKeys;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.stream.Collectors;


@Service
public class CommonFunctions {
	
	@Autowired
	HttpService httpService;

	private static final Logger logger = LoggerFactory.getLogger(CommonFunctions.class);

	@SuppressWarnings("unchecked")
	public Map<String, Object> convertObjectToMap(Object obj) {
		ObjectMapper oMapper = new ObjectMapper();
		oMapper.findAndRegisterModules().disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

		Map<String, Object> map = oMapper.convertValue(obj, Map.class);
		logger.info("Convert Object to Map: ", map);
		return map;
	}

    
	/**
	 * Convert non-draft(Manage) Quality Notifications Structure to draft Quality
	 * Notifications
	 * 
	 * @param manageQualityNotifications
	 * @return
	 */
    public cds.gen.qualitynotificationservice.QualityNotifications convertManageQualitynotificationtoQualityNotifications(
        cds.gen.managequalitynotificationservice.QualityNotifications manageQualityNotifications) {
    cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications = Struct
            .create(cds.gen.qualitynotificationservice.QualityNotifications.class);
    Map<String, Object> mConvertedData = manageQualityNotifications.entrySet().stream()
            .filter(entry -> Objects.nonNull(entry.getValue()))
            .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
    qualityNotifications.putAll(mConvertedData);
    return qualityNotifications;
}

	/**
	 * Convert non-draft Complaints Structure to draft Complaints
	 * 
	 * @param manageComplaints
	 * @return
	 */
	public cds.gen.complaintservice.Complaints convertManageComplaintsToComplaints(Complaints manageComplaints) {
		cds.gen.complaintservice.Complaints complaints = Struct.create(cds.gen.complaintservice.Complaints.class);
		Map<String, Object> mConvertedData = manageComplaints.entrySet().stream()
				.filter(entry -> Objects.nonNull(entry.getValue()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
		complaints.putAll(mConvertedData);

		return complaints;

	}


    public List<cds.gen.qualitynotificationservice.BusinessObjectStatuses> convertManageBOStatusToBusinessObjectStatus(
        List<cds.gen.managequalitynotificationservice.BusinessObjectStatuses> businessObjectStatuses) {
            List<cds.gen.qualitynotificationservice.BusinessObjectStatuses> boList=new ArrayList<>();
            businessObjectStatuses.forEach(bo->{
                cds.gen.qualitynotificationservice.BusinessObjectStatuses boStatuses = Struct
                        .create(cds.gen.qualitynotificationservice.BusinessObjectStatuses.class);
                Map<String, Object> mConvertedData = bo.entrySet().stream()
                        .filter(entry -> Objects.nonNull(entry.getValue()))
                        .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
                boStatuses.putAll(mConvertedData);
                boList.add(boStatuses);
            });
            
            return boList;
}
	
	/**
	 * Convert non-draft (Manage) Defects Structure to draft Defects
	 * 
	 * @param manageDefects
	 * @return
	 */
	public cds.gen.qualitynotificationservice.Defects convertManageDefectsToDraftDefects(cds.gen.managequalitynotificationservice.Defects manageDefects) {
		cds.gen.qualitynotificationservice.Defects defects = Struct
				.create(cds.gen.qualitynotificationservice.Defects.class);
		logger.info( "Manage Defects: ", manageDefects);
		if(null!=manageDefects) {
			Map<String, Object> mConvertedData = manageDefects.entrySet().stream()
					.filter(entry -> Objects.nonNull(entry.getValue()))
					.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
			defects.putAll(mConvertedData);
		}
		
		return defects;
	}

	/**
	 * Convert non-draft Defects Structure to draft Defects
	 * 
	 * @param manageDefects
	 * @return
	 */
	public cds.gen.claimservice.Claims convertManageClaimsToClaims(Claims manageClaims) {
		cds.gen.claimservice.Claims claims = Struct.create(cds.gen.claimservice.Claims.class);
		Map<String, Object> mConvertedData = manageClaims.entrySet().stream()
				.filter(entry -> Objects.nonNull(entry.getValue()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
		claims.putAll(mConvertedData);
		return claims;
	}
	
	/**
	 * Convert non-draft ReturnPurchaseOrders Structure to draft ReturnPurchaseOrders
	 * 
	 * @param manageReturnPurchaseOrders
	 * @return
	 */
	public cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders convertManageReturnPOtoReturnOrders(
			ReturnPurchaseOrders manageReturnPurchaseOrders){
		cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders returnPurchaseOrders = Struct.create(cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders.class);
		Map<String, Object> mConvertedData = manageReturnPurchaseOrders.entrySet().stream()
				.filter(entry -> Objects.nonNull(entry.getValue()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
		returnPurchaseOrders.putAll(mConvertedData);
		return returnPurchaseOrders;
	}
	
	/**
	 * Convert non-draft Supplier8DProcesses Structure to draft Supplier8DProcesses
	 * 
	 * @param manageSupplierIssueProcesses
	 * @return
	 */
	public cds.gen.supplierissueprocessservice.Supplier8DProcesses convertManageSupplierEightDToSupplierEightD(
			Supplier8DProcesses manageSupplierIssueProcesses){
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcesses = Struct.create(cds.gen.supplierissueprocessservice.Supplier8DProcesses.class);
		Map<String, Object> mConvertedData = manageSupplierIssueProcesses.entrySet().stream()
				.filter(entry -> Objects.nonNull(entry.getValue()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
		supplierIssueProcesses.putAll(mConvertedData);
		return supplierIssueProcesses;
    }
	
	/**
	 * Convert non-draft Supplier8DProcesses Structure to draft Supplier8DProcesses
	 * 
	 * @param internalSupplierIssueProcesses
	 * @return
	 */
	public cds.gen.supplierissueprocessservice.Supplier8DProcesses convertInternalSupplierEightDToSupplierEightD(
			cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses internalSupplierIssueProcesses){
		cds.gen.supplierissueprocessservice.Supplier8DProcesses supplierIssueProcesses = Struct.create(cds.gen.supplierissueprocessservice.Supplier8DProcesses.class);
		Map<String, Object> mConvertedData = internalSupplierIssueProcesses.entrySet().stream()
				.filter(entry -> Objects.nonNull(entry.getValue()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
		supplierIssueProcesses.putAll(mConvertedData);
		return supplierIssueProcesses;
    }
	
    /**
	 * Convert draft Claims Structure to non-draft Claims
	 * 
	 * @param claimServiceClaims
	 * @return
	 */
    public Claims convertClaimsToManageClaims(cds.gen.claimservice.Claims claimServiceClaims) {
					Claims claims =
                            Struct.create(Claims.class);                         
					Map<String, Object> mConvertedData = claimServiceClaims.entrySet().stream()
                            .filter(entry -> Objects.nonNull(entry.getValue()) && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.IS_ACTIVE_ENTITY))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.HAS_ACTIVE_ENTITY))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.HAS_DRAFT_ENTITY))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.IS_CLAIM_FIELD_CONTROL))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.IS_UPDATE_RESTRICTED))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.S_NAVIGATION))
		            && (!entry.getKey().equalsIgnoreCase(cds.gen.claimservice.Claims.NUMBER)))
                            .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
                            
                    logger.info("[CommonFunctions][convertClaimsToManageClaims] mConvertedData={}", mConvertedData);  
					claims.putAll(mConvertedData);
					return claims;
      }


    /**
	 * Convert draft Complaints Structure to non-draft Complaints
	 * 
	 * @param draftComplaints
	 * @return
	 */
      public cds.gen.managecomplaintservice.Complaints convertComplaintToManageComplaint(cds.gen.complaintservice.Complaints draftComplaints) {
					cds.gen.managecomplaintservice.Complaints manageComplaints =
                            Struct.create(cds.gen.managecomplaintservice.Complaints.class);                            
					Map<String, Object> mConvertedData = draftComplaints.entrySet().stream()
                            .filter(entry -> Objects.nonNull(entry.getValue()) && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_ACTIVE_ENTITY))
							&& (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_UPDATE_RESTRICTED))
							&& (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_COMPLAINT_NEW))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_FIELD_CONTROL_MANDATORY))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_HIDE_ADAPT_STREAMS))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_HIDE_COST_COLLECTION))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_SHOW_STREAMS))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.HAS_ACTIVE_ENTITY))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.HAS_DRAFT_ENTITY))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_HIDE_CLOSE_COMPLAINT))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_HIDE_REOPEN_COMPLAINT))
                            && (!entry.getKey().equalsIgnoreCase(cds.gen.complaintservice.Complaints.IS_HIDE_DISCARD_COMPLAINT)))
                            .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
                            
                    logger.info("[CommonFunctions][convertComplaintToManageComplaint] mConvertedData={}", mConvertedData);  
					manageComplaints.putAll(mConvertedData);
					return manageComplaints;
      }
      
      /**
	 * Convert ManageComplaints CostCollector Structure to CostCollectors
	 * 
	 * @param manageComplaintsCostCollector
	 * @return
	 */
	public cds.gen.costcollectorservice.CostCollectors convertManageCostCollectorToCostCollector(CostCollectors manageCostCollectors) {
		cds.gen.costcollectorservice.CostCollectors costCollectors = Struct.create(cds.gen.costcollectorservice.CostCollectors.class);
		Map<String, Object> mConvertedData = manageCostCollectors.entrySet().stream()
				.filter(entry -> Objects.nonNull(entry.getValue()))
				.collect(Collectors.toMap(Entry::getKey, Entry::getValue));
		costCollectors.putAll(mConvertedData);
		return costCollectors;
	}
	
	/**
	 * Set the request for Document flow object
	 * @param qualityNotificationNumber
	 * @param sourceObjectType
	 * @param returnOrderNumber
	 * @param targetObjectType
	 * @param sourceLogicalSystem
	 * @param targetLogicalSystem
	 * @return
	 */
	public DocumentFlow setDocumentFlowRequest(String qualityNotificationNumber, String sourceObjectType, 
			String returnOrderNumber, String targetObjectType, String sourceLogicalSystem,String targetLogicalSystem) {
		DocumentFlow docFlow = new DocumentFlow();
		 BinaryRelationObject objectA = new BinaryRelationObject();
         BinaryRelationObject objectB = new BinaryRelationObject();
		docFlow.setObjectANumber(qualityNotificationNumber);
		docFlow.setObjectAType(sourceObjectType);
		docFlow.setObjectBNumber(returnOrderNumber);
		docFlow.setObjectBType(targetObjectType);
		docFlow.setObjectA(objectA);
		docFlow.setObjectB(objectB);
		docFlow.setSourceLogicalSystem(sourceLogicalSystem);
		docFlow.setTargetLogicalSystem(targetLogicalSystem);
		return docFlow;
	}
	
    /**
     * Set the request for BinaryRelationDataModel object
     * @param qualityNotificationNumber
     * @param sourceObjectType
     * @param targetObjectType
     * @param sourceLogicalSystem
     * @param targetLogicalSystem
     * @return
     */
	public BinaryRelationDataModel setBinaryRelationDataModel(String qualityNotificationNumber, String sourceObjectType,String targetObjectType,String sourceLogicalSystem, String targetLogicalSystem) {
		DocumentFlow setDocumentFlowRequest = setDocumentFlowRequest(qualityNotificationNumber, sourceObjectType, "", 
				targetObjectType, sourceLogicalSystem, targetLogicalSystem);
		httpService.setBinaryRelationObjects(setDocumentFlowRequest);
		BinaryRelationDataModel binaryRelationDataModel = new BinaryRelationDataModel();
		binaryRelationDataModel.setObjectA(setDocumentFlowRequest.getObjectA());
		binaryRelationDataModel.setObjectB(setDocumentFlowRequest.getObjectB());
		binaryRelationDataModel.setRelationType(Constants.BINARY_RELATIONSHIP_TYPE);
		return binaryRelationDataModel;
	}
	
	/**
	 * Check if all attributes are selected
	 * in Address and Business Partner
	 * @param context
	 */
	public void checkBeforeRead(CdsReadEventContext context) {
		ParameterInfo parameterInfo = context.getParameterInfo();
		Map<String, String> map1 = parameterInfo.getQueryParams();
		String select = map1.get("$select");
		String expand = map1.get("$expand");
		logger.info("Select Param value {}" , select);	
		if(null!=select && select.contains("*")) {
		       throw new ServiceException(MessageKeys.INVALID_SELECTION_ATTRIBUTES);
		}else if(null!=expand && expand.contains("$select=*")) {
			logger.info("expand Param value {}" , expand);	 
			throw new ServiceException(MessageKeys.INVALID_SELECTION_ATTRIBUTES);
		}
	}

	/**
	 * Remove unwanted attributes for BusinessObjects Creation
	 * @param attributesList
	 */
	public void removeAttributes(List<String> attributesList,Map<String,Object> businessObjectMap) {
		for(String attribute : attributesList) {
			businessObjectMap.remove(attribute);
		}
	}


}
