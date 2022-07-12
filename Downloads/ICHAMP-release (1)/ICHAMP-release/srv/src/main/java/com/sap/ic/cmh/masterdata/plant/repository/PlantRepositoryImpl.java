package com.sap.ic.cmh.masterdata.plant.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.Plants_;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.StructuredType;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
public class PlantRepositoryImpl implements PlantRepository {

    public static final Logger logger = LoggerHelper.getLogger(PlantRepositoryImpl.class);

    @Autowired
    private PersistenceService db;
    @Autowired
    Messages messages;

    @Override
    public Map<String, String> getPlantMap(List<String> plantList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getPlantMap");
        CqnSelect plantSelect = Select.from(Plants_.class)
                .where(p -> p.plant().in(plantList));
        List<Plants> plantDbList = db.run(plantSelect).listOf(Plants.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getPlantMap");
        return plantDbList.stream().collect(Collectors.toMap(Plants::getId, Plants::getPlant));
    }

    @Override
    public List<String> getActiveComplaintsInPlant(List<String> plantsId) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInPlant");
        List<String> complaintsList = new ArrayList<>();
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.plant_ID().in(plantsId).and(b.complaintStatus_code()
                .ne(Constants.COMPLAINT_CLOSED)));
        final List<Complaints> complaints = db.run(complaintsSelect).listOf(Complaints.class);
        complaintsList.addAll(complaints.stream().map(Complaints::getPlantId).collect(Collectors.toList()));
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInPlant");
        return complaintsList;
    }

    @Override
    public String getPlantId(String plantCode) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getPlantId");
        CqnSelect plantSelect = Select.from(Plants_.class)
                .where(p -> p.plant().eq(plantCode));
        Plants plants = db.run(plantSelect).listOf(Plants.class).get(0);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getPlantId");
        return plants.getId();
    }

    @Override
    public void deletePlantList(List<String> recordsToBeDeleted) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteInactivePlantData");
        CqnDelete delete = Delete.from(Plants_.class).where(cc -> cc.ID().in(recordsToBeDeleted));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Records Deleted count: ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteInactivePlantData");
    }

    @Override
    public <E extends StructuredType<E>> Result fetchPlant(String plant, String message,
            Class<E> targetClass, Function<E, Object> targetClassAttribute) {
               
                LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "fetchPlant");
       
        Result plantResult = null;
        if (!ObjectUtils.isEmpty(plant)) {
            plantResult = (db.run(Select.from(Plants_.class).where(b -> b.plant().eq(plant))));
        } else {
            messages.error(message).target("in", targetClass, targetClassAttribute);
        }
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "fetchPlant");
        return plantResult;
    }
    

     /**
     * This method is used to get Plant details based on plant
     * @param plant
     * @return
     */
    @Override
    public Result fetchPlantDetailsBasedOnNumber(String plant) {
        return db.run(Select.from(Plants_.class).columns(Plants.ID,Plants.PLANT)
        .where(b -> b.plant().eq(plant)));
    }
}
