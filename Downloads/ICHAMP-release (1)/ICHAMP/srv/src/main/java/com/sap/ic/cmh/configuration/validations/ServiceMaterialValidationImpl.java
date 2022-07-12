package com.sap.ic.cmh.configuration.validations;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterials_;
import cds.gen.configurationservice.SubItemTypes_;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.services.messages.Message.Severity;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.SecurityValidator;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.cds.Row;

@Component
public class ServiceMaterialValidationImpl implements ServiceMaterialValidation {

    @Autowired
    ServiceMaterialDao serviceMaterialDao;

    @Autowired
    Messages messages;

    @Autowired
    ConfigurationFieldsValidation configurationFieldsValidation;

    @Autowired
    PersistenceService db;

    @Autowired
    SecurityValidator securityValidator;

    private static final Logger logger = LoggerFactory.getLogger(ServiceMaterialValidationImpl.class);
    private static final String SERVICE_MATERIAL_VALIDATION_IMPL = "ServiceMaterialValidationImpl";
    /**
     * Common method to call business validation of destination and sub-item attribute
     *
     * @param {@link ServiceMaterials} serviceMaterials
     *
     * @public
     */
    @Override
    public void validateServiceMaterials(ServiceMaterials serviceMaterial){
        LoggerHelper.logMethodEntry(logger, SERVICE_MATERIAL_VALIDATION_IMPL, "validateServiceMaterials");
        if(!validateItemAndSubItem(serviceMaterial.getItemTypeCode(),serviceMaterial.getSubItemTypeCode())){
            messages.error(MessageKeys.ITEM_TYPE_NOT_MATCH).target("in",
                    ServiceMaterials_.class,
                    ServiceMaterials_::subItemType_code);
        }
        if(!configurationFieldsValidation.validateDestination(serviceMaterial.getDestination())){
                messages.error(MessageKeys.DESTINATION_IS_MANDATORY).target("in", ServiceMaterials_.class,
                        ServiceMaterials_::destination);
        }
        if(!configurationFieldsValidation.validateDestinationValue(serviceMaterial.getDestination())){
            messages.error(MessageKeys.DESTINATION_IS_INVALID).target("in", ServiceMaterials_.class,
            ServiceMaterials_::destination);
        }
        if(serviceMaterial.getServiceMaterial()==null || serviceMaterial.getServiceMaterial().isEmpty()){
            messages.error(MessageKeys.SERVICE_MATERIAL_IS_MANDATORY).target("in", ServiceMaterials_.class,
                    ServiceMaterials_::serviceMaterial);
        }
        if(!configurationFieldsValidation.validateItemType(serviceMaterial.getItemTypeCode())){
            messages.error(MessageKeys.ITEM_TYPE_IS_MANDATORY).target("in", ServiceMaterials_.class,
                    ServiceMaterials_::itemType_code);
        }
        if(!configurationFieldsValidation.validateSubItemType(serviceMaterial.getSubItemTypeCode())){
            messages.error(MessageKeys.SUB_ITEM_TYPE_IS_MANDATORY).target("in",
                    ServiceMaterials_.class,
                    ServiceMaterials_::subItemType_code);
        }
        if(serviceMaterial.getDescription()!=null && !securityValidator.isValidText(serviceMaterial.getDescription())){
            messages.error(MessageKeys.INVALID_SERVICE_MATERIAL_DESCRIPTION).target("in", ServiceMaterials_.class,
                    ServiceMaterials_::description);
        }
        if (messages.stream().noneMatch(message -> message.getSeverity() == Severity.ERROR)) {
            Result serviceMaterials = serviceMaterialDao.getServiceMaterialsBasedOnDestinationAndSubItemType(serviceMaterial.getDestination(), serviceMaterial.getSubItemTypeCode());
            Optional<Row> serviceMaterialsFirst = serviceMaterials.first();
            if(serviceMaterialsFirst.isPresent() && !serviceMaterial.getId().equals(serviceMaterialsFirst.get().get("ID").toString())) {
                messages.error(MessageKeys.ONLY_ONE_SERVICE_MATERIAL_PER_DESTINATION).target("in", ServiceMaterials_.class,
                        ServiceMaterials_::destination);
            }
        }
        LoggerHelper.logMethodExit(logger, SERVICE_MATERIAL_VALIDATION_IMPL, "validateServiceMaterials");
    }
    public boolean validateItemAndSubItem(String itemType, String subItem){
        logger.info("Item Type is {}",itemType);
        logger.info("Sub Item is {}",subItem);
        Result subItemType = db.run(Select.from(SubItemTypes_.class)
                .where(b -> b.code().eq(subItem)));
        if(subItemType.first().isPresent()){
            for (Row row : subItemType) {
                String sItemType = row.get("itemType_code").toString();
                if(itemType.equals(sItemType)){
                    logger.info("Item Type for selected SubItem is {}",sItemType);
                    return true;
                }
              }
        }
        return false;
    }
}
