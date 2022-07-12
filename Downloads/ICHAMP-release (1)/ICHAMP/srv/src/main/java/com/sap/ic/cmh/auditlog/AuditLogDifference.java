package com.sap.ic.cmh.auditlog;

import java.lang.reflect.Type;
import com.google.gson.reflect.TypeToken;
import com.sap.ic.cmh.utils.LoggerHelper;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;
import org.javers.core.Javers;
import org.javers.core.JaversBuilder;
import org.javers.core.diff.Diff;
import org.javers.core.diff.ListCompareAlgorithm;
import org.javers.core.diff.changetype.map.MapChange;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class AuditLogDifference<T> {
    @Autowired AuditOldDataContext<T> auditOldDataContext;

    public static final Logger logger = LoggerHelper.getLogger(AuditLogDifference.class);

    public void setOldData(T oldData) {
        auditOldDataContext.set(oldData);
    }

    public List<ObjectDiff> getDifference(T newData) {
        Javers javers =
                JaversBuilder.javers()
                        .withListCompareAlgorithm(ListCompareAlgorithm.LEVENSHTEIN_DISTANCE)
                        .build();

        Gson gson = new Gson();

        String oldjson = gson.toJson(auditOldDataContext.get());
        String newjson = gson.toJson(newData);

        List<ObjectDiff> diffList = new ArrayList<>();

        Type mapType = new TypeToken<Map<String, Object>>() {}.getType();
        Map<String, Object> oldMap = gson.fromJson(oldjson, mapType);
        Map<String, Object> newMap = gson.fromJson(newjson, mapType);

        Diff diff = javers.compare(oldMap, newMap);
        logger.info("--------------oldMap---------" + oldMap);
        logger.info("--------------newMap---------" + newMap);
        auditOldDataContext.reset();

        diff.getChanges()
                .forEach(
                        change -> {
                            if (change instanceof MapChange) {
                                if(oldMap != null){
                                    ((MapChange) change)
                                            .getEntryValueChanges()
                                            .forEach(
                                                    valueChange ->
                                                            diffList.add(
                                                                    new ObjectDiff(
                                                                            valueChange
                                                                                    .getKey()
                                                                                    .toString(),
                                                                            valueChange
                                                                                    .getLeftValue()
                                                                                    .toString(),
                                                                            valueChange
                                                                                    .getRightValue()
                                                                                    .toString())));
                                            if (oldMap.size() < newMap.size()) {
                                                 ((MapChange) change)
                                                        .getEntryAddedChanges()
                                                            .forEach(
                                                                valueChange ->
                                                                    diffList.add(
                                                                        new ObjectDiff(
                                                                            valueChange
                                                                                .getKey()
                                                                                .toString(),
                                                                                 null,
                                                                                valueChange
                                                                                .getValue()
                                                                                .toString())));
                                                                                    }
                                            if(oldMap.size() > newMap.size()){
                                                ((MapChange) change)
                                            .getEntryRemovedChanges()
                                            .forEach(
                                                    valueChange ->
                                                            diffList.add(
                                                                    new ObjectDiff(
                                                                            valueChange
                                                                                    .getKey()
                                                                                    .toString(),
                                                                            valueChange
                                                                                    .getValue()
                                                                                    .toString(),
                                                                            null)));
                                            }
                                }
                                else{
                                    ((MapChange) change)
                                                        .getEntryAddedChanges()
                                                            .forEach(
                                                                valueChange ->
                                                                    diffList.add(
                                                                        new ObjectDiff(
                                                                            valueChange
                                                                                .getKey()
                                                                                .toString(),
                                                                                 null,
                                                                                valueChange
                                                                                .getValue()
                                                                                .toString())));
                                }
                                 
                            }
                        });

        return diffList;
    }
    
}
