package com.sap.ic.cmh.configuration.validations;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.businesspartner.repository.BusinessPartnerRepository;
import com.sap.ic.cmh.masterdata.distributionchannel.persistency.DistributionChannelRepository;
import com.sap.ic.cmh.masterdata.division.persistency.DivisionRepository;
import com.sap.ic.cmh.masterdata.salesorganization.persistency.SalesOrganizationRepository;
import com.sap.ic.cmh.masterdata.unitofmeasure.service.UnitOfMeasureService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.complaintservice.BTPUsers;
import cds.gen.complaintservice.BusinessPartners_;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.UnitOfMeasures;
import cds.gen.masterdataservice.UnitOfMeasures_;
import cds.gen.qualitynotificationservice.QualityNotifications_;


@Component
public class MasterDataValidationImpl implements MasterDataValidation {

	private static final Logger logger = LoggerFactory.getLogger(MasterDataValidationImpl.class);
	private static final String MASTER_DATA_VALIDATION_IMPL = "MasterDataValidationImpl";

	@Autowired
	ConfigurationDao configurationDao;
	@Autowired
	Messages messages;
	@Autowired
	UnitOfMeasureService unitOfMeasureService;
	@Autowired
	BusinessPartnerRepository businessPartnerDao;
	@Autowired
	ComplaintService complaintService;
    @Autowired
    SalesOrganizationRepository salesOrganizationRepository;
    @Autowired
    DistributionChannelRepository distributionChannelRepository;
    @Autowired
    DivisionRepository divisionRepository;

	/**
	 * Validate Material
	 * 
	 * @param materialId
	 * @return
	 */
	public void validateMaterial(String materialId) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validateMaterial");
		String sMessageKey = "";
		if (materialId == null) {
			sMessageKey = MessageKeys.MATERIAL_NOT_MAINTAINED;
		}
		if (materialId != null) {
			Result materialDataResult = configurationDao.getMaterialData(materialId);
			MaterialMasterGeneralDatas materialData =materialDataResult.first().isPresent() ? materialDataResult.listOf(MaterialMasterGeneralDatas.class).get(0) : null;
			if (null == materialData) {
				sMessageKey = MessageKeys.INVALID_MATERIAL;
			}
		}
		if (!sMessageKey.isEmpty()) {
				messages.error(sMessageKey).target("in", QualityNotifications_.class, qnotif -> qnotif.material_ID());
			}
		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateMaterial");
	}

	/**
	 * Validate company code
	 * 
	 * @param companyId
	 * @return
	 */
	public void validateCompanyCode(String companyId) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validateCompanyCode");
		String sMessageKey = "";
		if (companyId == null) {
			sMessageKey = MessageKeys.COMPANY_CODE_NOT_MAINTAINED;
		}
		if (companyId != null) {
			Result companyCodesResult = configurationDao.getCompanyCodes(companyId);
			CompanyCodes companyCodes = companyCodesResult.first().isPresent() ? companyCodesResult.listOf(CompanyCodes.class).get(0) : null;
			if (null == companyCodes) {
				sMessageKey = MessageKeys.INVALID_COMPANY_CODE;
			}
		}
		if (!sMessageKey.isEmpty()) {
			 		messages.error(sMessageKey).target("in", QualityNotifications_.class, qnotif -> qnotif.company_ID());
				}
		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateCompanyCode");

	}

	/**
	 * Validate plant
	 * 
	 * @param plantId
	 * @return
	 */
	public void validatePlant(String plantId) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validatePlant");
		String sMessageKey = "";
		if (plantId == null) {
			sMessageKey = MessageKeys.PLANT_NOT_MAINTAINED;
		}
		if (plantId != null) {
			Result plantDataResult = configurationDao.getPlantData(plantId);
			Plants plantData = plantDataResult.first().isPresent() ? plantDataResult.listOf(Plants.class).get(0) : null;
			if (null == plantData) {
				sMessageKey = MessageKeys.INVALID_PLANT;
			}	
		}
		if (!sMessageKey.isEmpty()) {
			messages.error(sMessageKey).target("in", QualityNotifications_.class, qnotif -> qnotif.plant_ID());
				}

		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validatePlant");
		
	}

	/**
	 * Validate Supplier
	 * 
	 * @param supplierId
	 * @return
	 */
	public void validateSupplier(String supplierId) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validateSupplier");
		String sMessageKey = "";
		if (supplierId == null) {
			sMessageKey = MessageKeys.SUPPLIER_NOT_MAINTAINED;
		}
		if (supplierId != null) {
			Result supplierDataResult = configurationDao.getSupplierData(supplierId);
			BusinessPartners supplierData = supplierDataResult.first().isPresent() ? supplierDataResult.listOf(BusinessPartners.class).get(0) : null;
			if (null == supplierData) {
				sMessageKey = MessageKeys.INVALID_SUPPLIER;
			}else if(Boolean.TRUE.equals(supplierData.getIsMarkedForDeletion())) {
				sMessageKey = MessageKeys.BUSINESS_PARTNER_MARKED_FOR_DELETION;
			}
		}
		if (!sMessageKey.isEmpty()) {
			 		messages.error(sMessageKey).target("in", QualityNotifications_.class, qnotif -> qnotif.supplier_ID());
			 	}
		
		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateSupplier");
	}

	/**
	 * Validate Purchase organization
	 * 
	 * @param purchasingOrganizationId
	 * @return
	 */
	public void validatePurchaseOrg(String purchasingOrganizationId) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validatePurchaseOrg");
		String sMessageKey = "";
		if (purchasingOrganizationId == null) {
			sMessageKey = MessageKeys.PURCHASE_ORG_NOT_MAINTAINED;
		}
		if (purchasingOrganizationId != null) {
			Result purchOrgDataResult = configurationDao.getPurchOrgData(purchasingOrganizationId);
			PurchaseOrganizations purchOrgData = purchOrgDataResult.first().isPresent() ? purchOrgDataResult.listOf(PurchaseOrganizations.class).get(0) : null;
			if (null == purchOrgData) {
				sMessageKey = MessageKeys.INVALID_PURCHASE_ORG;
			}
		}
		if (!sMessageKey.isEmpty()) {
				messages.error(sMessageKey).target("in", QualityNotifications_.class,
			 				qnotif -> qnotif.purchasingOrganization_ID());
			 	}
		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validatePurchaseOrg");
	}

	/**
	 * Validate Quantity
	 * 
	 * @param quantity
	 * @return
	 */
	public void validateQuantity(BigDecimal quantity) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validateQuantity");
		String sMessageKey = "";
		if (quantity==null || quantity.compareTo(BigDecimal.ZERO) == 0) {
			sMessageKey = MessageKeys.QUANTITY_NOT_MAINTAINED;
		}
		if (!sMessageKey.isEmpty()) {
			 		messages.error(sMessageKey).target("in", QualityNotifications_.class,
			 				qnotif -> qnotif.quantity());
			 	}
		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateQuantity");
		
	}
    /**
     * Validate unit of measure
     */
	public void validUnitOfMeasure(String unit) {
		LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validUnitOfMeasure");
		String sMessageKey = "";
		if(unit==null) {
			sMessageKey = MessageKeys.UNIT_NOT_MAINTAINED;
		}else {
			UnitOfMeasures unitOfMeasure = unitOfMeasureService.getUnitOfMeasureDetails(unit);
			if(null==unitOfMeasure) {
				sMessageKey = MessageKeys.INVALID_UNIT;
			}
		}
		if(!sMessageKey.isEmpty()) {
			messages.error(sMessageKey).target("in", UnitOfMeasures_.class,
	 				u->u.code());
		}
		LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validUnitOfMeasure");
	}

	/**
	 * Validate Supplier Contact Person
	 *
	 * @param businessPartnerId
	 */
	@Override
	public void validateSupplierPersonType(String businessPartnerId) {
		Result businessPartnersResult = businessPartnerDao.getBusinessPartners(businessPartnerId);
		if(businessPartnersResult.first().isPresent()){
			String selectedBusinessPartnerType = businessPartnersResult.first().get().get("businessPartnerType").toString();
			String isMarkedForDelete = businessPartnersResult.first().get().get("isMarkedForDeletion").toString();
			if(!selectedBusinessPartnerType.equals(Constants.EXCHANGE_PARTNER_TYPE_SUPPLER_CONTACT)){
				messages.error(MessageKeys.WRONG_SUPPLIER_PERSON_SELECTED).target("in", BusinessPartners_.class,u->u.businessPartnerType());
			}
			if(isMarkedForDelete.equals("true")){
				messages.error(MessageKeys.SUPPLIER_PERSON_CANNOT_BE_ADDED).target("in", BusinessPartners_.class,u->u.businessPartnerType());
			}
		}
	}

	@Override
	public boolean validateBTPUser(String personResponsible){
        List<BTPUsers> responsiblePersonList = complaintService.getAllResponsiblePerson();
		return !CollectionUtils.isEmpty(responsiblePersonList) && responsiblePersonList.stream()
					.anyMatch(rp -> rp.getPersonResponsibleId()!=null && rp.getPersonResponsibleId().equals(personResponsible));
	}
	
	/**
	 * validate sales organization
	 * 
	 * @param {@link String, Class<E>, Function<E, Object>} 
	 * salesOrganizationId, targetClass, targetClassAttribute
	 * 
	 * @public
	 */
	@Override
	public <E extends StructuredType<E>> void validateSalesOrganization(String salesOrganizationId, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validateSalesOrganization");
        if(salesOrganizationId==null){
        	messages.error(MessageKeys.SALES_ORGANIZATION_IS_MANDATORY).target("in",targetClass, targetClassAttribute);
        }
        else if(Boolean.FALSE.equals(checkSalesOrganizationExist(salesOrganizationId))){
        	messages.error(MessageKeys.INVALID_SALES_ORGANIZATION).target("in",targetClass, targetClassAttribute);
        }     
        LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateSalesOrganization"); 	
	}
	


    /**
	 * validate distribution channel
	 * 
	 * @param {@link String, Class<E>, Function<E, Object>} 
	 * distributionChannelId, targetClass, targetClassAttribute
	 * 
	 * @public
	 */
	@Override
	public <E extends StructuredType<E>> void validateDistributeChannel(String distributionChannelId,String salesOrganizationId, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateDistributeChannel");
        if(distributionChannelId==null){
        	messages.error(MessageKeys.DISTRIBUTION_CHANNEL_IS_MANDATORY).target("in",targetClass, targetClassAttribute);
        }else {
        	Optional<Row> distributionChannel=distributionChannelRepository.getDistributionChannelById(distributionChannelId).first();
        	if(distributionChannel.isPresent() && !distributionChannel.get().get("salesOrganizationID_ID").toString().equals(salesOrganizationId)){	
        		messages.error(MessageKeys.DISTRIBUTION_CHANNEL_AND_SALES_ORGANIZATION_ARE_NOT_ASSOCIATED).target("in",targetClass, targetClassAttribute);
        	}else if(!distributionChannel.isPresent()){
        		messages.error(MessageKeys.INVALID_DISTRIBUTION_CHANNEL).target("in",targetClass, targetClassAttribute);
        	}
        }
        LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateDistributeChannel");	
	}
	
	
    /**
	 * validate division 
	 * 
	 * @param {@link String, Class<E>, Function<E, Object>} 
	 * divisionId, targetClass, targetClassAttribute
	 * 
	 * @public
	 */
	@Override
	public <E extends StructuredType<E>> void validateDivision(String divisionId,String salesOrganizationId, Class<E> targetClass, Function<E, Object> targetClassAttribute) {
        LoggerHelper.logMethodEntry(logger, MASTER_DATA_VALIDATION_IMPL, "validateDivision");
        if(divisionId==null){
        	messages.error(MessageKeys.SALES_DIVISION_IS_MANDATORY).target("in",targetClass, targetClassAttribute);
        }else {
        	Optional<Row> division=divisionRepository.getDivisionIDandSalesIDById(divisionId).first();
        	if(division.isPresent() && !division.get().get("salesOrganizationID_ID").toString().equals(salesOrganizationId)){	
        			messages.error(MessageKeys.DIVISION_AND_SALES_ORGANIZATION_ARE_NOT_ASSOCIATED).target("in",targetClass, targetClassAttribute);
        	}else if(!division.isPresent()){
        		messages.error(MessageKeys.INVALID_DIVISION).target("in",targetClass, targetClassAttribute);
        	}
        }
        LoggerHelper.logMethodExit(logger, MASTER_DATA_VALIDATION_IMPL, "validateDivision");	
	}
	
    /**
	 * check if sales organization exists 
	 * 
	 * @param {@link String} itemCategoryId
	 */
	@Override
	public boolean checkSalesOrganizationExist(String salesOrganizationId) {
		Result salesOrganizationResult=salesOrganizationRepository.getSalesOrganizationById(salesOrganizationId);
		return salesOrganizationResult.first().isPresent();
	}

}
