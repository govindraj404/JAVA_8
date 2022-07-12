package com.sap.ic.cmh.masterdata.common.persistency;

import cds.gen.masterdataservice.*;
import cds.gen.sap.common.Currencies;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

@Component
public class MasterDataDao {

    @Autowired
    private PersistenceService db;

    public static final Logger logger = LoggerHelper.getLogger(MasterDataDao.class);

    /**
     * This method is used to get the Currencies
     *
     * @param currenciesCode
     */
    public Result getCurrencies(String currenciesCode) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getCurrencies");
        Result aCurrencies = null;
        if (!ObjectUtils.isEmpty(currenciesCode)) {
            aCurrencies = (db.run(Select.from(Currencies_.class).where(b -> b.code().eq(currenciesCode))));
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getCurrencies");
        return aCurrencies;
    }

    /**
     * This method is used to get the Country COde
     *
     * @param countryKeyCode
     * @return Result
     */
    public Result getCountryCode(String countryKeyCode) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getCountryCode");
        Result aCountries = null;
        if (!ObjectUtils.isEmpty(countryKeyCode)) {
            aCountries = (db.run(Select.from(Countries_.class).where(b -> b.code().eq(countryKeyCode))));
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getCountryCode");
        return aCountries;
    }

    /**
     * Get Material
     *
     * @param materialID
     * @return MaterialMasterGeneralDatas
     */
    public MaterialMasterGeneralDatas getMaterial(String materialID) {
        CqnSelect select = Select.from(MaterialMasterGeneralDatas_.class)
                .where(b -> b.get(MaterialMasterGeneralDatas.ID).eq(materialID));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(MaterialMasterGeneralDatas.class).get(0)
                : null;
    }


    /**
     * Get Supplier
     *
     * @param supplierID
     * @return BusinessPartners
     */
    public BusinessPartners getSupplier(String supplierID) {
        CqnSelect select = Select.from(BusinessPartners_.class)
                .where(b -> b.get(BusinessPartners.ID).eq(supplierID));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(BusinessPartners.class).get(0) : null;
    }


    /**
     * Get Plant
     *
     * @param plantID
     * @return Plants
     */
    public Plants getPlant(String plantID) {
        CqnSelect select = Select.from(Plants_.class).where(b -> b.get(Plants.ID).eq(plantID));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(Plants.class).get(0) : null;
    }


    /**
     * Get Company Code based on Plant
     *
     * @param companyCode
     * @return Plants
     */
    public Plants validateCompanyCodeBasedOnPlant(String companyCode) {
        CqnSelect select =
                Select.from(Plants_.class).where(b -> b.get(Plants.COMPANY_CODE).eq(companyCode));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(Plants.class).get(0) : null;
    }

    /**
     * Get Company Code based on Plant
     * 
     * @param plantId
     * @return Plants
     */
    public Plants getCompanyCodeBasedOnPlants(String plantId) {
        CqnSelect select = Select.from(Plants_.class).where(b -> b.get(Plants.ID).eq(plantId));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(Plants.class).get(0) : null;
    }

    /**
     * Get Company based on Company code
     * 
     * @param companyCode
     * @return Plants
     */
    public CompanyCodes getCompanyCodeID(String companyCode) {
        CqnSelect select = Select.from(CompanyCodes_.class)
                .where(b -> b.get(CompanyCodes.COMPANY_CODE).eq(companyCode));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(CompanyCodes.class).get(0) : null;
    }


    /**
     * Get Company Code
     *
     * @param companyCodeID
     * @return CompanyCodes
     */
    public CompanyCodes getCompanyCode(String companyCodeID) {
        CqnSelect select = Select.from(CompanyCodes_.class)
                .where(b -> b.get(CompanyCodes.ID).eq(companyCodeID));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(CompanyCodes.class).get(0) : null;
    }



    /**
     * Get Purchasing Organization
     *
     * @param purchasingOrganizationID
     * @return PurchaseOrganizations
     */
    public PurchaseOrganizations getPurchasingOrganization(String purchasingOrganizationID) {
        CqnSelect select = Select.from(PurchaseOrganizations_.class)
                .where(b -> b.get(PurchaseOrganizations.ID).eq(purchasingOrganizationID));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(PurchaseOrganizations.class).get(0)
                : null;
    }



    /**
     * Get Currency
     *
     * @param currency
     * @return Currencies
     */
    public Currencies getCurrency(String currency) {
        CqnSelect select =
                Select.from(cds.gen.sap.common.Currencies_.class).where(b -> b.get(Currencies.CODE).eq(currency));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(Currencies.class).get(0) : null;
    }


    /**
     * Get Currency Based On Plant
     *
     * @param companyID
     * @return CompanyCodes
     */
    public CompanyCodes getCurrencyBasedOnCompanyCodes(String companyID) {
        CqnSelect select =
                Select.from(CompanyCodes_.class).where(b -> b.get(CompanyCodes.ID).eq(companyID));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(CompanyCodes.class).get(0) : null;
    }

    /**
     * validate Currency Based On Plant
     * 
     * @param currency
     * @return CompanyCodes
     */
    public CompanyCodes validateCurrencyBasedOnCompanyCode(String currency) {
        CqnSelect select = Select.from(CompanyCodes_.class)
                .where(b -> b.get(CompanyCodes.CURRENCY_CODE).eq(currency));
        Result result = db.run(select);
        return result.first().isPresent() ? result.listOf(CompanyCodes.class).get(0) : null;
    }


    /**
     * This method is used to get the DefectGroup
     *
     * @param defectCode
     * @return Result
     */
    public Result getDefectGroup(String defectCode) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getDefectGroup");
        Result defectGroup = null;
        if (!ObjectUtils.isEmpty(defectCode)) {
            defectGroup = (db.run(Select.from(DefectGroups_.class).where(b -> b.code().eq(defectCode))));
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getDefectGroup");
        return defectGroup;
    }
    
    /**
     * This method is used to get the Item Type Code
     *
     * @param itemTypeKeyCode
     * @return Result
     */
    public Result getItemTypeCode(String itemTypeKeyCode) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getItemTypeCode");
        Result aItemTypes = null;
        if (!ObjectUtils.isEmpty(itemTypeKeyCode)) {
            aItemTypes = (db.run(Select.from(ItemTypes_.class).where(b -> b.code().eq(itemTypeKeyCode))));
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getItemTypeCode");
        return aItemTypes;
    }

    /**
     * Get Action Based on BusinessObject Type
     * 
     * @param bObject
     * @return Result
     */
    public Result getAction(String bObject) {
        return db.run(Select.from(Actions_.class)
                .where(b -> b.get(Actions.BUSINESS_OBJECT_TYPE_CODE).eq(bObject)));
    }


    /**
     * get ActionPrecondition based on Action Code
     * 
     * @param actionCode
     * @return Result
     */
    public Result getActionPrecondition(String actionCode) {
        return db.run(Select.from(ActionPreconditions_.class)
                .where(b -> b.get(ActionPreconditions.CODE_CODE).eq(actionCode)));
    }

}