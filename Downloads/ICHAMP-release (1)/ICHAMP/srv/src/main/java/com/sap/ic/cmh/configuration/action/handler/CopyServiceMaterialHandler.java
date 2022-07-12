package com.sap.ic.cmh.configuration.action.handler;

import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterials_;
import cds.gen.configurationservice.ServiceMaterialUnits;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Stream;
import com.sap.cds.Result;
import java.util.Map;
import java.util.HashMap;
import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.sap.cds.services.draft.DraftService;
import org.springframework.beans.factory.annotation.Qualifier;
import com.sap.ic.cmh.configuration.action.context.CopyServiceMaterialContext;

@Component
@ServiceName("ConfigurationService")
public class CopyServiceMaterialHandler implements EventHandler {

    @Autowired
    ServiceMaterialUnitDao serviceMaterialUnitDao;

    @Autowired
    Messages messages;

    private final DraftService draftService;

    CopyServiceMaterialHandler(@Qualifier("ConfigurationService") DraftService draftService) {
        this.draftService = draftService;
    }

    @On(entity = ServiceMaterials_.CDS_NAME)
    public void copyServiceMaterial(final CopyServiceMaterialContext context) {

        CqnSelect select = context.getCqn();
        CdsService service = context.getService();
        ServiceMaterials serviceMaterials = (service).run(select)
                .single(ServiceMaterials.class);
        ServiceMaterials copyServiceMaterial = Struct.create(ServiceMaterials.class);

        copyServiceMaterial.setSubItemTypeCode(serviceMaterials.getSubItemTypeCode());
        copyServiceMaterial.setDestination(serviceMaterials.getDestination());
        copyServiceMaterial.setItemTypeCode(serviceMaterials.getItemTypeCode());
        copyServiceMaterial.setServiceMaterial(serviceMaterials.getServiceMaterial());
        copyServiceMaterial.setDescription(serviceMaterials.getDescription());
        Result serviceMaterialUnitResult = serviceMaterialUnitDao.getServiceMaterialUnitsBasedOnServiceMaterial(serviceMaterials.getId());
        if (serviceMaterialUnitResult.rowCount() > 0) {
            List<Map<String, Object>> serviceMaterialUnitList = new ArrayList<>();
            Stream<ServiceMaterialUnits> serviceMaterialUnitStream = serviceMaterialUnitResult.streamOf(ServiceMaterialUnits.class);
			serviceMaterialUnitStream.forEach(b -> {
                Map<String, Object> copiedServiceMaterialUnit = new HashMap<>();
                copiedServiceMaterialUnit.put("unit_code",b.getUnitCode());
                copiedServiceMaterialUnit.put("numerator",b.getNumerator());
                copiedServiceMaterialUnit.put("denominator",b.getDenominator());
                copiedServiceMaterialUnit.put("defaultUnit",b.getDefaultUnit());
                serviceMaterialUnitList.add(copiedServiceMaterialUnit);
            });
        copyServiceMaterial.setServiceMaterialUnit(serviceMaterialUnitList);
        }
        context.setResult(draftService.newDraft(Insert.into(ServiceMaterials_.class).entry(copyServiceMaterial))
                .single(ServiceMaterials.class));
        context.setCompleted();

    }
}