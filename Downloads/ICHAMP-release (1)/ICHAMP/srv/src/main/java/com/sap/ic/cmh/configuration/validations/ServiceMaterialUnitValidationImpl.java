package com.sap.ic.cmh.configuration.validations;

import java.util.List;
import java.math.BigDecimal;
import com.sap.cds.Result;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.sap.ic.cmh.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterials_;
import cds.gen.configurationservice.ServiceMaterialUnits;
import cds.gen.configurationservice.ServiceMaterialUnits_;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.SecurityValidator;


@Component
public class ServiceMaterialUnitValidationImpl implements ServiceMaterialUnitValidation {

    @Autowired
    Messages messages;

    @Autowired
    SecurityValidator securityValidator;

    @Autowired
    ConfigurationService configurationService;

    @Autowired
    ServiceMaterialUnitDao serviceMaterialUnitDao;

    private static final Logger logger = LoggerFactory.getLogger(ServiceMaterialUnitValidationImpl.class);
    private static final String SERVICE_MATERIAL_UNIT_VALIDATION_IMPL = "ServiceMaterialUnitValidationImpl";


    /**
     * Common method to call business validation of destination and sub-item attribute
     *
     * @param {@link ServiceMaterials} serviceMaterials
     *
     * @public
     */
    @Override
    public void validateServiceMaterialUnits(ServiceMaterials serviceMaterial){
        LoggerHelper.logMethodEntry(logger, SERVICE_MATERIAL_UNIT_VALIDATION_IMPL, "validateServiceMaterialUnits");
        List<ServiceMaterialUnits> serviceMaterialUnits = serviceMaterial.getServiceMaterialUnit();
        if(serviceMaterialUnits!=null && !serviceMaterialUnits.isEmpty()){
            if(!serviceMaterial.getItemTypeCode().equals(Constants.ITEM_TYPE_FR)){
                messages.error(MessageKeys.UNIT_NOT_VALID_FOR_ITEM_TYPE).target("in", ServiceMaterials_.class,
                ServiceMaterials_::itemType_code);
            }
        validateDefaultUnit(serviceMaterialUnits);
        for(ServiceMaterialUnits serviceMaterialUnit : serviceMaterialUnits){
            validateServiceMaterialUnitsNotNull(serviceMaterialUnit);
        }
    }
    }

    public void validateServiceMaterialUnitsNotNull(ServiceMaterialUnits serviceMaterialUnit){

        if(serviceMaterialUnit.getServiceMaterialId()==null || serviceMaterialUnit.getServiceMaterialId().isEmpty()){
            messages.error(MessageKeys.SERVICE_MATERIAL_IS_MANDATORY).target("in", ServiceMaterialUnits_.class,
            ServiceMaterialUnits_::serviceMaterial_ID);
        }
        if(serviceMaterialUnit.getUnitCode()==null || serviceMaterialUnit.getUnitCode().isEmpty()){
            messages.error(MessageKeys.UNIT_IS_MANDATORY).target("in", ServiceMaterialUnits_.class,
            ServiceMaterialUnits_::unit_code);
        }
        if(serviceMaterialUnit.getNumerator()==null){
            messages.error(MessageKeys.NUMERATOR_IS_MANDATORY).target("in", ServiceMaterialUnits_.class,
            ServiceMaterialUnits_::numerator);
        }
        if(serviceMaterialUnit.getDenominator()==null){
            messages.error(MessageKeys.DENOMINATOR_IS_MANDATORY).target("in", ServiceMaterialUnits_.class,
            ServiceMaterialUnits_::denominator);
        }

        if(serviceMaterialUnit.getServiceMaterialId()!=null && serviceMaterialUnit.getUnitCode()!=null){
            Result serviceMatUnit = serviceMaterialUnitDao.getServiceMaterialUnitBasedOnMaterialIdAndUnit(serviceMaterialUnit.getServiceMaterialId(),serviceMaterialUnit.getUnitCode());
            if(serviceMatUnit.first().isPresent() && !serviceMaterialUnit.getId().equals(serviceMatUnit.first().get().get("ID").toString())){
                messages.error(MessageKeys.SERVICE_MATERIAL_UNIT_CONFIGURATION_EXISTS).target("in", ServiceMaterialUnits_.class,
                ServiceMaterialUnits_::serviceMaterial_ID);
            }  
        }

        if(serviceMaterialUnit.getDenominator()!=null && serviceMaterialUnit.getDenominator().compareTo(new BigDecimal(0))<=0){
            messages.error(MessageKeys.DENOMINATOR_IS_INVALID).target("in", ServiceMaterialUnits_.class,
            ServiceMaterialUnits_::denominator);
        }

        if(serviceMaterialUnit.getNumerator()!=null && serviceMaterialUnit.getNumerator().compareTo(new BigDecimal(0))<0){
            messages.error(MessageKeys.NUMERATOR_IS_INVALID).target("in", ServiceMaterialUnits_.class,
            ServiceMaterialUnits_::numerator);
        }
    }

    public void validateDefaultUnit(List<ServiceMaterialUnits> serviceMaterialUnits){
        int count = 0;
        if(!serviceMaterialUnits.isEmpty()){
            for(ServiceMaterialUnits serviceMaterialUnit : serviceMaterialUnits){
                if(Boolean.TRUE.equals(serviceMaterialUnit.getDefaultUnit())){
                    count++;
                }
            }
            if(count==0){
                messages.error(MessageKeys.NO_DEFAULT_UNIT_SELECTED).target("in", ServiceMaterialUnits_.class,
                ServiceMaterialUnits_::defaultUnit);
            } 
            else if(count>1){
                messages.error(MessageKeys.DEFAULT_UNIT_MORE_THAN_ONE).target("in", ServiceMaterialUnits_.class,
                ServiceMaterialUnits_::defaultUnit);
            }
        }
    }
}
