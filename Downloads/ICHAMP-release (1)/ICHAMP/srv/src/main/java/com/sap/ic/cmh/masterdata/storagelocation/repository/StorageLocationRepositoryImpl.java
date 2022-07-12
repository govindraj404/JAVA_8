package com.sap.ic.cmh.masterdata.storagelocation.repository;

import cds.gen.masterdataservice.StorageLocations;
import cds.gen.masterdataservice.StorageLocations_;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
public class StorageLocationRepositoryImpl implements StorageLocationRepository {

    public static final Logger logger = LoggerHelper.getLogger(StorageLocationRepositoryImpl.class);

    @Autowired
    private PersistenceService db;

    @Override
    public Map<String, String> getStorageLocationMap(List<String> storageLocations, List<String> plantCodeList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getStorageLocationMap");
        CqnSelect storageLocationSelect = Select.from(StorageLocations_.class)
                .where(st -> st.storageLocation().in(storageLocations)
                        .and(st.plantCode().in(plantCodeList)));
        List<StorageLocations> storageLocationList = db.run(storageLocationSelect).listOf(StorageLocations.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getStorageLocationMap");
        return storageLocationList.stream().collect(Collectors.toMap(StorageLocations::getId, StorageLocations::getStorageLocation));
    }

    @Override
    public void deleteStorageLocationList(List<String> storageLocationList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deleteStorageLocationList");
        CqnDelete delete = Delete.from(StorageLocations_.class).where(st -> st.ID().in(storageLocationList));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Records Deleted count: ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deleteStorageLocationList");
    }

    @Override
    public Result fetchStorageLocations(String storageLocation, String plantId) {
       
        return db.run(Select.from(StorageLocations_.class)
        .where(b -> b.storageLocation().eq(storageLocation)
                .and(b.plantCodeID_ID().eq(plantId))));
    }
}
