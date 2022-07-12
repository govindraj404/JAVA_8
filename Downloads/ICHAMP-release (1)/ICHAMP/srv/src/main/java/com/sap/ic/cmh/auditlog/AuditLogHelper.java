package com.sap.ic.cmh.auditlog;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.sap.cds.CdsData;
import com.sap.cds.services.auditlog.*;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.javers.core.Javers;
import org.javers.core.JaversBuilder;
import org.javers.core.diff.Diff;
import org.javers.core.diff.ListCompareAlgorithm;
import org.javers.core.diff.changetype.map.MapChange;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.sap.cds.services.request.UserInfo;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.ic.cmh.utils.Constants;
import com.sap.xs.audit.api.exception.AuditLogException;

@Component
public class AuditLogHelper<T>  {


    private final Logger logger = LoggerFactory.getLogger(AuditLogHelper.class);
    private static final String AUDITLOG_HELPER = "AuditLogHelper";

    @Autowired
    AuditOldDataContext<T> auditOldDataContext;

    @Autowired
    private AuditLogService auditLogService;

    @Autowired
    private AuditLoggerService auditLoggerService;

    @Autowired
    private CdsRuntime cdsRuntime;

    public void setOldData(T oldData) {
        auditOldDataContext.set(oldData);
    }

    private AuditLogModel prepareAuditLogModel(String operation, Map<String, String> entityInfoMap, ObjectDiff diff) {
        UserInfo userInfo = cdsRuntime.getProvidedUserInfo();
        String tenant = userInfo.getTenant();
        String user = userInfo.getName();

        // filter out the roles only for current entity
        Set<String> filteredRoles =
                userInfo.getRoles().stream()
                        .filter(r -> r.contains(entityInfoMap.get(Constants.ENTITY_NAME)))
                        .collect(Collectors.toSet());
        String roles = filteredRoles.toString();
        return new AuditLogModel(user, tenant, operation, roles, entityInfoMap, diff);
    }

    public void logDeleteAuditData(ObjectDiff diff, Map<String, String> entityInfoMap) {
        logger.debug("Triggering async logging.");
        AuditLogModel auditLogModel =
                prepareAuditLogModel(Constants.DELETE_OP, entityInfoMap, diff);
        try {

            auditLoggerService.logActionModificationAuditData(
                    auditLogModel);

        } catch (AuditLogException e) {
            logger.error("Audit logs could not be updated for {}",entityInfoMap.get(Constants.ENTITY_NAME),e);
        }


        logger.debug("Async logging triggered.");
    }



    public void logUpsertAuditData(List<ObjectDiff> diffList, Map<String, String> entityInfoMap) {
        diffList.forEach(
                diff -> {
                    AuditLogModel auditLogModel =
                            prepareAuditLogModel(Constants.UPSERT_OP, entityInfoMap, diff);
                    try {
                        auditLoggerService.logActionModificationAuditData(
                                auditLogModel);
                    } catch (AuditLogException e) {
                        logger.error("Audit logs could not be updated for {}",entityInfoMap.get(Constants.ENTITY_NAME),e);
                    }

                });
        logger.info("Async logging triggered.");
    }

    public Map<String, String> buildEntityInfoMap(
            String entityName, String uuid) {
        Map<String, String> entityInfoMap = new HashMap<>();
        entityInfoMap.put(Constants.ENTITY_NAME, entityName);
        entityInfoMap.put(Constants.OBJECT_ID, uuid);
        return entityInfoMap;
    }

    public void logDelete(String entityName, String id) {
        ObjectDiff diffList = null;
        Map<String, String> entityInfoMap =
                buildEntityInfoMap(
                        entityName, id);
        logDeleteAuditData(diffList, entityInfoMap);
    }

    /**
     * @param keyName
     * @param value
     * @return keyValuePair
     */
    public KeyValuePair createKeyValuePairId(String keyName, String value) {
        LoggerHelper.logMethodEntry(logger, AUDITLOG_HELPER, "createKeyValuePairId");
        KeyValuePair id = KeyValuePair.create();
        id.setKeyName(keyName);
        id.setValue(value);
        LoggerHelper.logMethodExit(logger, AUDITLOG_HELPER, "createKeyValuePairId");
        return id;
    }

    /**
     * @param type
     * @param keyValuePair
     * @return dataObject
     */
    public DataObject createDataObject(String type, KeyValuePair keyValuePair) {
        LoggerHelper.logMethodEntry(logger, AUDITLOG_HELPER, "createDataObject");
        DataObject dataObject = DataObject.create();
        dataObject.setType(type);
        dataObject.setId(Arrays.asList(keyValuePair));
        LoggerHelper.logMethodExit(logger, AUDITLOG_HELPER, "createDataObject");
        return dataObject;
    }

    /**
     * @param dataObject
     * @param changedAttribute
     * @return configChange
     */
    public ConfigChange createConfigChange(DataObject dataObject, ChangedAttribute changedAttribute) {
        LoggerHelper.logMethodEntry(logger, AUDITLOG_HELPER, "createConfigChange");
        ConfigChange cfgChange = ConfigChange.create();
        cfgChange.setDataObject(dataObject);
        cfgChange.setAttributes(Collections.singletonList(changedAttribute));
        LoggerHelper.logMethodExit(logger, AUDITLOG_HELPER, "createConfigChange");
        return cfgChange;
    }

    /**
     * @param name
     * @param newValue
     * @param oldValue
     * @return changedAttribute
     */
    public ChangedAttribute createChangedAttribute(String name, String newValue, String oldValue) {
        LoggerHelper.logMethodEntry(logger, AUDITLOG_HELPER, "createChangedAttribute");
        ChangedAttribute attribute = ChangedAttribute.create();
        attribute.setName(name);
        attribute.setOldValue(oldValue);
        attribute.setNewValue(newValue);
        LoggerHelper.logMethodExit(logger, AUDITLOG_HELPER, "createChangedAttribute");
        return attribute;
    }

    /**
     * @param newOblect
     * @return diffList
     */
    public List<ChangedAttribute> getChangedAttributeDifference(Object newOblect) {
        LoggerHelper.logMethodEntry(logger, AUDITLOG_HELPER, "getChangedAttributeDifference");
        Javers javers = JaversBuilder.javers().withListCompareAlgorithm(ListCompareAlgorithm.LEVENSHTEIN_DISTANCE)
                .build();
        List<ChangedAttribute> diffList = new ArrayList<>();

        Gson gson = new Gson();

        String oldjson = gson.toJson(auditOldDataContext.get());
        String newjson = gson.toJson(newOblect);

        Type mapType = new TypeToken<Map<String, Object>>() {}.getType();
        Map<String, Object> oldMap = gson.fromJson(oldjson, mapType);
        Map<String, Object> newMap = gson.fromJson(newjson, mapType);

        Diff diff = javers.compare(oldMap, newMap);
        logger.info("--------------oldMap---------" + oldMap);
        logger.info("--------------newMap---------" + newMap);
        auditOldDataContext.reset();

        diff.getChanges().forEach(change -> {
            if (change instanceof MapChange) {
                if (oldMap != null && newMap != null) {
                    ((MapChange) change).getEntryValueChanges()
                            .forEach(valueChange -> diffList.add(createChangedAttribute(valueChange.getKey().toString(),
                                    valueChange.getRightValue().toString(), valueChange.getLeftValue().toString())));
                } else if (oldMap == null && newMap != null) {
                    ((MapChange) change).getEntryAddedChanges()
                            .forEach(valueChange -> diffList.add(createChangedAttribute(valueChange.getKey().toString(),
                                    valueChange.getValue().toString(), null)));
                } else {
                    ((MapChange) change).getEntryRemovedChanges()
                            .forEach(valueChange -> diffList.add(createChangedAttribute(valueChange.getKey().toString(),
                                    null, valueChange.getValue().toString())));
                }
            }
        });
        LoggerHelper.logMethodExit(logger, AUDITLOG_HELPER, "getChangedAttributeDifference");
        return diffList;
    }
    /**
     * @param entity
     * @param action
     * @param newData
     */
    public <R extends CdsData> void logConfigChange(String entity, Action action, R newData) {
        LoggerHelper.logMethodEntry(logger, AUDITLOG_HELPER, "logConfigChange");
        logger.info("createConfigChange:: AuditLog data --------------STARTED--------------");
        String id = newData != null && newData.get("ID") != null ? newData.get("ID").toString() : "";
        KeyValuePair keyValuePair = createKeyValuePairId("ID", id);
        DataObject dataObject = createDataObject(entity, keyValuePair);
        List<ChangedAttribute> changedAttributes = getChangedAttributeDifference(newData);
        changedAttributes.forEach(
                changedAttribute -> {
                    ConfigChange cfgChange = createConfigChange(dataObject, changedAttribute);
                    auditLogService.logConfigChange(action, cfgChange);
                });
        logger.info("createConfigChange:: AuditLog data --------------ENDED--------------");
    }

}