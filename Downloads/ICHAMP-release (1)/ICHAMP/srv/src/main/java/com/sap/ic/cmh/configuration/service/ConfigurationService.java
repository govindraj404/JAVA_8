package com.sap.ic.cmh.configuration.service;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import com.sap.cds.Struct;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.persistency.ConfigurationDao;
import com.sap.ic.cmh.configuration.validations.MasterDataValidation;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;

import cds.gen.configurationservice.Destinations;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas_;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchaseOrganizations_;
import io.vavr.control.Try;
import com.sap.ic.cmh.configuration.persistency.BusinessObjectConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ClaimStatusMappingsDao;
import com.sap.ic.cmh.configuration.persistency.ConditionTypeDao;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import cds.gen.configurationservice.BusinessObjectConfigurations;
import cds.gen.configurationservice.ClaimStatusMappings;
import cds.gen.configurationservice.ConditionTypes;
import cds.gen.configurationservice.DestinationConfigurations;
import cds.gen.configurationservice.ServiceMaterials;

@Service
public class ConfigurationService {

	@Autowired
	ConfigurationDao configurationDao;

	@Autowired
	Messages messages;

	@Autowired
	MasterDataValidation masterDataValidation;

	@Autowired
	DestinationService destinationService;

    @Autowired
    BusinessObjectConfigurationDao businessObjectConfigurationDao;

    @Autowired
    ClaimStatusMappingsDao claimStatusMappingsDao;

    @Autowired
    DestinationConfigurationDao destinationConfigurationDao;

    @Autowired
    ServiceMaterialDao serviceMaterialDao;

    @Autowired
    ConditionTypeDao conditionTypeDao;


	private static final String CONFIGURATION_SERVICE = "ConfigurationService";
	private static final Logger logger = LoggerFactory.getLogger(ConfigurationService.class);

	/**
	 * For a particular BO's master data ids, get the relevant master data details
	 * to pass these to the CPI flow (e.g. plant name,material and Supplier number)
	 * 
	 * @param materialId
	 * @param plantId
	 * @param supplierId
	 * @param purchaseOrgId
	 * @param companyId
	 * @return
	 */
	public MasterData getMasterDataDetails(String materialId, String plantId, String supplierId, String purchaseOrgId) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_SERVICE, "getMasterDataDetails");
		MasterData masterData = new MasterData();
		Result materialDataResult = configurationDao.getMaterialData(materialId);

		MaterialMasterGeneralDatas materialData = materialDataResult.first().isPresent()
				? materialDataResult.listOf(MaterialMasterGeneralDatas.class).get(0)
				: null;
		if (null != materialData) {
			masterData.setMaterial(materialData);
		}
		Result plantDataResult = configurationDao.getPlantData(plantId);
		Plants plantData = plantDataResult.first().isPresent() ? plantDataResult.listOf(Plants.class).get(0) : null;
		if (null != plantData) {
			masterData.setPlants(plantData);
		}
		Result supplierDataResult = configurationDao.getSupplierData(supplierId);
		BusinessPartners supplierData = supplierDataResult.first().isPresent()
				? supplierDataResult.listOf(BusinessPartners.class).get(0)
				: null;
		if (null != supplierData) {
			masterData.setSupplier(supplierData);
		}
		Result purchOrgDataResult = configurationDao.getPurchOrgData(purchaseOrgId);
		PurchaseOrganizations purchOrgData = purchOrgDataResult.first().isPresent()
				? purchOrgDataResult.listOf(PurchaseOrganizations.class).get(0)
				: null;
		if (null != purchOrgData) {
			masterData.setPurchaseOrg(purchOrgData);
		}

		LoggerHelper.logMethodEntry(logger, CONFIGURATION_SERVICE, "getMasterDataDetails");
		return masterData;
	}

	/**
	 * To get company code details
	 * 
	 * @param companyCodeId
	 * @return
	 */
	public CompanyCodes getCompanyCodes(String companyCodeId) {
		Result companyCodesResult = configurationDao.getCompanyCodes(companyCodeId);
		return companyCodesResult.first().isPresent() ? companyCodesResult.listOf(CompanyCodes.class).get(0) : null;
	}

	/**
	 * To get person responsible detail
	 * 
	 * @param personResponsibleId
	 * @return
	 */
	public BusinessPartners getSupplier(String personResponsibleId) {
		Result supplierDataResult = configurationDao.getSupplierData(personResponsibleId);
		return supplierDataResult.first().isPresent() ? supplierDataResult.listOf(BusinessPartners.class).get(0) : null;
	}

	/**
	 * Get master data IDs by codes
	 * 
	 * @param materialCode
	 * @param plant
	 * @param supplierName
	 * @param personResponsible
	 * @param purchaseOrg
	 * @return
	 */
	public MasterData getMasterDataDetailsByCodes(String materialCode, String plant, String supplierName,
			String personResponsible, String purchaseOrg) {
		LoggerHelper.logMethodEntry(logger, CONFIGURATION_SERVICE, "getMasterDataDetailsByCodes");
		MasterData masterData = new MasterData();

		validateMaterial(materialCode, masterData);
		validatePlant(plant, masterData);
		validateSupplier(supplierName, masterData);
		validatePersonResponsible(personResponsible, masterData);
		validatePurchaseOrganisation(purchaseOrg, masterData);
		return masterData;
	}

	/**
	 * Validate PurchaseOrganisation
	 * 
	 * @param purchaseOrg
	 * @param masterData
	 */
	public void validatePurchaseOrganisation(String purchaseOrg, MasterData masterData) {
		if (StringUtils.isNotBlank(purchaseOrg)) {
			Result purchaseOrgResult = configurationDao.getPurchOrgDataBasedOnPurchOrg(purchaseOrg);
			PurchaseOrganizations purchaseOrgBasedOnNumber = purchaseOrgResult.first().isPresent()
					? purchaseOrgResult.listOf(PurchaseOrganizations.class).get(0)
					: null;
			if (null != purchaseOrgBasedOnNumber) {
				masterData.setPurchaseOrg(purchaseOrgBasedOnNumber);
			} else {
				messages.error(MessageKeys.PURCHASE_ORGANIZATION_DOES_NOT_EXIST).target("in",
						PurchaseOrganizations_.class, PurchaseOrganizations_::purchaseOrganization);
			}
		}
	}

	/**
	 * Validate PersonResponsible
	 * 
	 * @param personResponsible
	 * @param masterData
	 */
	public void validatePersonResponsible(String personResponsible, MasterData masterData) {
		if (StringUtils.isNotBlank(personResponsible)) {
			Result personResponsibleResult = configurationDao.getPersonResponsibleBasedOnNumber(personResponsible);
			BusinessPartners personResponsibleBasedOnNumber = personResponsibleResult.first().isPresent()
					? personResponsibleResult.listOf(BusinessPartners.class).get(0)
					: null;
			if (null != personResponsibleBasedOnNumber) {
				masterData.setPersonResponsible(personResponsibleBasedOnNumber);
			} else {
				messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", BusinessPartners_.class,
						BusinessPartners_::businessPartnerNumber);
			}
		} else {
			messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", BusinessPartners_.class,
					BusinessPartners_::businessPartnerNumber);
		}

	}

	/**
	 * Validate Supplier
	 * 
	 * @param supplierName
	 * @param masterData
	 */
	public void validateSupplier(String supplierName, MasterData masterData) {
		if (StringUtils.isNotBlank(supplierName)) {
			Result supplierResult = configurationDao.getSupplierDataBasedOnNumber(supplierName);
			BusinessPartners supplierDataBasedOnNumber = supplierResult.first().isPresent()
					? supplierResult.listOf(BusinessPartners.class).get(0)
					: null;
			if (null != supplierDataBasedOnNumber) {
				masterData.setSupplier(supplierDataBasedOnNumber);
			} else {
				messages.error(MessageKeys.BUSINESS_PARTNER_DOES_NOT_EXIST).target("in", BusinessPartners_.class,
						BusinessPartners_::businessPartnerNumber);
			}
		} else {
			messages.error(MessageKeys.BUSINESS_PARTNER_DOES_NOT_EXIST).target("in", BusinessPartners_.class,
					BusinessPartners_::businessPartnerNumber);
		}

	}

	/**
	 * Validate Plant
	 * 
	 * @param plant
	 * @param masterData
	 */
	public void validatePlant(String plant, MasterData masterData) {
		if (StringUtils.isNotBlank(plant)) {
			Result plantResult = configurationDao.getPlantDataBasedOnPlant(plant);
			Plants plantDataBasedOnPlant = plantResult.first().isPresent() ? plantResult.listOf(Plants.class).get(0)
					: null;
			if (null != plantDataBasedOnPlant) {
				masterData.setPlants(plantDataBasedOnPlant);
			} else {
				messages.error(MessageKeys.PLANT_DOES_NOT_EXIST).target("in", Plants_.class, Plants_::plant);
			}
		} else {
			messages.error(MessageKeys.PLANT_IS_MANDATORY).target("in", Plants_.class, Plants_::plant);
		}

	}

	/**
	 * Validate Material
	 * 
	 * @param materialCode
	 * @param masterData
	 */
	public void validateMaterial(String materialCode, MasterData masterData) {
		if (StringUtils.isNotBlank(materialCode)) {
			Result materialResult = configurationDao.getMaterialDataBasedOnCode(materialCode);
			MaterialMasterGeneralDatas materialDataBasedOnCode = materialResult.first().isPresent()
					? materialResult.listOf(MaterialMasterGeneralDatas.class).get(0)
					: null;
			if (null != materialDataBasedOnCode) {
				masterData.setMaterial(materialDataBasedOnCode);
			} else {
				messages.error(MessageKeys.MATERIAL_MASTER_GENERAL_DATA_DOES_NOT_EXIST).target("in",
						MaterialMasterGeneralDatas_.class, MaterialMasterGeneralDatas_::materialCode);
			}
		}
	}

	/**
	 * Validate Supplier Contact Person
	 * 
	 * @param supplierName
	 */
	public BusinessPartners validateSupplierContactPerson(String supplierName) {
		if (StringUtils.isNotBlank(supplierName)) {
			Result supplierResult = configurationDao.getSupplierDataBasedOnNumber(supplierName);
			BusinessPartners supplierDataBasedOnNumber = supplierResult.first().isPresent()
					? supplierResult.listOf(BusinessPartners.class).get(0)
					: null;
			if (null != supplierDataBasedOnNumber) {
				masterDataValidation.validateSupplierPersonType(supplierDataBasedOnNumber.getId());
				return supplierDataBasedOnNumber;
			} else {
				messages.error(MessageKeys.SUPPLIER_PERSON_DOES_NOT_EXIST).target("in", BusinessPartners_.class,
						BusinessPartners_::businessPartnerNumber);
			}
		}
		return null;
	}

	/**
	 * Validate PersonResponsible
	 * 
	 * @param personResponsible
	 * @param masterData
	 */
	public BusinessPartners validatePersonResponsibleCode(String personResponsibleCode) {
		if (StringUtils.isNotBlank(personResponsibleCode)) {
			Result personResponsibleResult = configurationDao.getPersonResponsibleBasedOnNumber(personResponsibleCode);
			BusinessPartners personResponsibleBasedOnNumber = personResponsibleResult.first().isPresent()
					? personResponsibleResult.listOf(BusinessPartners.class).get(0)
					: null;
			if (null != personResponsibleBasedOnNumber) {
				return personResponsibleBasedOnNumber;
			} else {
				messages.error(MessageKeys.PERS_RESP_IS_NOT_VALID).target("in", BusinessPartners_.class,
						BusinessPartners_::businessPartnerNumber);
			}
		}
		return null;
	}

	public List<Destinations> getallDestinationsFromBTP() {
		ScpCfDestinationLoader scpCfDestinationLoader = new ScpCfDestinationLoader();
		Try<Iterable<ScpCfDestination>> scpcfDestinations = destinationService
				.getallDestination(scpCfDestinationLoader);
		Iterator<ScpCfDestination> scpcfdestination = scpcfDestinations.get().iterator();
		List<Destinations> destinationList = new ArrayList<>();
		while (scpcfdestination.hasNext()) {
			Destinations dest = Struct.create(Destinations.class);
			ScpCfDestination getDestination = scpcfdestination.next();
			dest.setDestination(getDestination.getName());
			destinationList.add(dest);
		}
		return destinationList;
	}

    public BusinessObjectConfigurations getBusinessObjectConfigurationsDetails(String id){

        Result businessObjectConfigurationsDetailsResult = businessObjectConfigurationDao.getBusinessObjectConfigurationsDetails(id);
		return businessObjectConfigurationsDetailsResult.first().isPresent() ? businessObjectConfigurationsDetailsResult.listOf(BusinessObjectConfigurations.class).get(0)
				: null;
    }

    public ClaimStatusMappings getclaimStatusMappingsDetails(String id){
        Result claimStatusMappingsDetailsResult = claimStatusMappingsDao.getClaimStatusMappingDetails(id);
        return claimStatusMappingsDetailsResult.first().isPresent() ? claimStatusMappingsDetailsResult.listOf(ClaimStatusMappings.class).get(0)
        : null;
    }

    public ConditionTypes getConditionTypeDetail(String id){
        Result conditionTypesResult = conditionTypeDao.getConditionTypesDetail(id);
        return conditionTypesResult.first().isPresent() ? conditionTypesResult.listOf(ConditionTypes.class).get(0)
        : null;
    }

    public DestinationConfigurations getDestinationConfigDetails(String id){
        Result destinationConfigResult = destinationConfigurationDao.getDestinationConfigDetail(id);
        return destinationConfigResult.first().isPresent() ? destinationConfigResult.listOf(DestinationConfigurations.class).get(0)
        : null;
    }

    public ServiceMaterials getServiceMaterialsDetails(String id){
        Result serviceMaterialsResult = serviceMaterialDao.getServiceMaterialsDetail(id);
        return serviceMaterialsResult.first().isPresent() ? serviceMaterialsResult.listOf(ServiceMaterials.class).get(0)
        : null;
    }

    public PurchaseOrganizations getPurchaseOrganisation(String purchaseOrg) {
		Result purchaseOrgResult = configurationDao.getPurchOrgDataBasedOnPurchOrg(purchaseOrg);
		return purchaseOrgResult.first().isPresent()
				? purchaseOrgResult.listOf(PurchaseOrganizations.class).get(0)
				: null;
	}
}