package com.sap.ic.cmh.masterdata.address.repository;

import cds.gen.masterdataservice.*;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;


@Component
public class AddressRepositoryImpl implements AddressRepository {
    public static final Logger logger = LoggerHelper.getLogger(AddressRepositoryImpl.class);
    @Autowired
    private PersistenceService db;
    @Autowired
    private Messages messages;

    @Override
    public Map<String, String> getAddressMap(List<String> addresses) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getAddressMap");
        CqnSelect addressSelect = Select.from(Addresses_.class).columns("ID", "address")
                .where(b -> b.address().in(addresses));
        List<Addresses> addressList = db.run(addressSelect).listOf(Addresses.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getAddressMap");
        return addressList.stream().collect(Collectors.toMap(Addresses::getAddress, Addresses::getId));
    }

    @Override
    public Map<String, String> getCompanyCodeInAddress(List<String> addressIDList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getCompanyCodeInAddress");
        CqnSelect companyCodeSelect = Select.from(CompanyCodes_.class)
                .where(b -> b.address().in(addressIDList));
        List<CompanyCodes> companyCodeList = db.run(companyCodeSelect).listOf(CompanyCodes.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getCompanyCodeInAddress");
        return companyCodeList.stream().collect(Collectors.toMap(CompanyCodes::getAddress, CompanyCodes::getAddressIDId));
    }

    @Override
    public Map<String, String> getPlantDetails(List<String> addressIDList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getPlantDetails");
        CqnSelect plantsSelect = Select.from(Plants_.class)
                .where(b -> b.address().in(addressIDList));
        List<Plants> plantList = db.run(plantsSelect).listOf(Plants.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getPlantDetails");
        return plantList.stream().collect(Collectors.toMap(Plants::getAddress, Plants::getAddressIDId));
    }

    @Override
    public Map<String, String> getBusinessPartner(List<String> addressIDList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getBusinessPartner");
        CqnSelect businessPartnersSelect = Select.from(BusinessPartners_.class)
                .where(b -> b.address().in(addressIDList));
        List<BusinessPartners> businessPartnersList = db.run(businessPartnersSelect).listOf(BusinessPartners.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getBusinessPartner");
        return businessPartnersList.stream().collect(Collectors.toMap(BusinessPartners::getAddress, BusinessPartners::getAddressIDId));
    }

    @Override
    public void deleteAddressList(List<String> addressIdList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteAddressLists");
        CqnDelete delete = Delete.from(Addresses_.class).where(cc -> cc.address().in(addressIdList));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Addresses deleted count : ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteAddressLists");
    }

    /**
     * This method is used to get Address details based on Address
     * @param addressID
     * @return
     */
    @Override
    public Result getAddressDetailsBasedOnAddress(String address){
        return db.run(Select.from(Addresses_.class).columns(Addresses.ID).where(b -> b.get(Addresses.ADDRESS).eq(address)));
 
     }

     /**
     * This method is used to get Address details based on ID
     * @param addressID
     * @return
     */
    @Override
    public Result getAddressDetails(String addressID){
        return db.run(Select.from(Addresses_.class).where(b -> b.get(Addresses.ID).eq(addressID)));
 
     }

    @Override
    public <E extends StructuredType<E>> Result fetchAddress(String addressId, String message,
            Class<E> targetClass, Function<E, Object> targetClassAttribute) {
                Result result = null;
                if (!ObjectUtils.isEmpty(addressId)) {
                    result = (db.run(Select.from(Addresses_.class).where(b -> b.address().eq(addressId)))); 
                } else {
                    messages.error(message).target("in", targetClass, targetClassAttribute);
                }
        return result;
    }
}