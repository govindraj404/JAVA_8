package com.sap.ic.cmh.auditlog;

import cds.gen.com.sap.ic.cmh.referencetype.ReferenceTypes_;
import cds.gen.configurationservice.ReferenceTypes;
import cds.gen.qualitynotificationservice.QualityNotifications;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sap.cds.Struct;
import com.sap.cds.services.auditlog.*;
import com.sap.cds.services.request.UserInfo;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.xs.audit.api.exception.AuditLogException;
import org.javers.core.Changes;
import org.javers.core.Javers;
import org.javers.core.diff.Diff;
import org.javers.core.diff.changetype.map.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import com.sap.ic.cmh.utils.Constants;


public class AuditLogHelperTest {
    @InjectMocks @Autowired
    AuditLogHelper<ReferenceTypes> auditLogHelper;

    @Mock
    AuditLoggerService auditLoggerService;

    @Mock
    AuditOldDataContext<ReferenceTypes> auditOldDataContext;

    @Mock
    AuditLogService auditLogService;

    @Mock
    CdsRuntime cdsRuntime;

    @Mock
    UserInfo userInfo;

    @Mock
    Javers javers;

    @Mock
    MapChange mapChange;

    @Mock
    Diff diff;

    @Mock
    Changes changes;

    @Mock
    EntryChange entryChange;

    @Mock
    EntryValueChange entryValueChange;

    @Mock
    EntryAdded entryAdded;

    @Mock
    EntryRemoved entryRemoved;

    private ReferenceTypes referenceTypes;

    private cds.gen.configurationservice.ReferenceTypes referenceType;

    cds.gen.configurationservice.ReferenceTypes newReferenceType;
    cds.gen.configurationservice.ReferenceTypes oldReferenceType;

    KeyValuePair keyValuePair;
    DataObject dataObject;
    ConfigChange cfgChange;
    ChangedAttribute changedAttribute;
    List<ChangedAttribute> configChanges = new ArrayList<>();
    List<ChangedAttribute> changedAttributeList = new ArrayList<>();
    List<EntryChange> entryChanges = new ArrayList<>();
    List<EntryValueChange> entryValuesChanges = new ArrayList<>();
    List<EntryAdded> entryAddChanges = new ArrayList<>();
    List<EntryRemoved> entryRemovedChanges = new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        referenceTypes = Struct.create(ReferenceTypes.class);
        referenceTypes.setId("4a010676-d636-4577-b94f-f0c70ced1376");
        referenceTypes.setCode("RT");
        referenceTypes.setDescription("RT");
        referenceTypes.setReferenceDocumentCategoryCode("BILLDOC");
    }

    @Test
    public void TestSetOldData(){
        auditLogHelper.setOldData(ReferenceTypes.create());
    }

    @Test
    public void testLogDelete() {

        Set<String> expectedRoles = new HashSet<>();
        expectedRoles.add("TestEntityRole");
        Set<String> roles = new HashSet<>();
        roles.add("ROLE1");
        roles.add("ROLE2");
        roles.add("TestEntityRole");
        Mockito.when(cdsRuntime.getProvidedUserInfo()).thenReturn(userInfo);
        Mockito.when(userInfo.getTenant()).thenReturn("TEST-TENANT");
        Mockito.when(userInfo.getName()).thenReturn("USER-NAME");
        Mockito.when(userInfo.getRoles()).thenReturn(roles);
        ObjectDiff diff = new ObjectDiff();
        Map<String, String> entityInfo = new HashMap<>();
        entityInfo.put(Constants.ENTITY_NAME, "entityName");
        assertDoesNotThrow(
                () -> auditLogHelper.logDeleteAuditData(diff, entityInfo));
    }

    @Test
    public void testLogUpsert() {
        Set<String> expectedRoles = new HashSet<>();
        expectedRoles.add("TestEntityRole");
        Set<String> roles = new HashSet<>();
        roles.add("ROLE1");
        roles.add("ROLE2");
        roles.add("TestEntityRole");
        Mockito.when(cdsRuntime.getProvidedUserInfo()).thenReturn(userInfo);
        Mockito.when(userInfo.getTenant()).thenReturn("TEST-TENANT");
        Mockito.when(userInfo.getName()).thenReturn("USER-NAME");
        Mockito.when(userInfo.getRoles()).thenReturn(roles);
        QualityNotifications mockedObj = QualityNotifications.create();
        mockedObj.setId("test-id");
        List<ObjectDiff> diffList = new ArrayList<>();
        diffList.add(new ObjectDiff());
        Map<String, String> entityInfo = new HashMap<>();
        entityInfo.put(Constants.ENTITY_NAME, "entityName");
        assertDoesNotThrow(
                () -> auditLogHelper.logUpsertAuditData(diffList, entityInfo));
    }

    @Test
    public void testBuildEntityInfoMap() {
        Map<String, String> actualEntityInfoMap =
                auditLogHelper.buildEntityInfoMap(
                        "entityName", "UUID");
        Map<String, String> expectedEntityInfoMap = new HashMap<>();
        expectedEntityInfoMap.put(Constants.ENTITY_NAME, "entityName");
        expectedEntityInfoMap.put(Constants.OBJECT_ID, "UUID");
        assertEquals(expectedEntityInfoMap, actualEntityInfoMap);
    }

    @Test
    public void testCreateKeyValuePairId() {
        keyValuePair = KeyValuePair.create();
        keyValuePair = auditLogHelper.createKeyValuePairId(ReferenceTypes.ID, referenceTypes.getId());
    }

    @Test
    public void testCreateDataObject() {
        dataObject = DataObject.create();
        dataObject = auditLogHelper.createDataObject(ReferenceTypes_.CDS_NAME, keyValuePair);
    }

    @Test
    public void testCreateChangedAttribute() {
        changedAttribute = ChangedAttribute.create();
        changedAttribute = auditLogHelper.createChangedAttribute(ReferenceTypes.CODE,
                referenceTypes != null ? referenceTypes.getCode() : null, null);
    }

    @Test
    public void testCreateConfigChange() {
        cfgChange = ConfigChange.create();
        cfgChange = auditLogHelper.createConfigChange(dataObject, changedAttribute);
    }

    @Test
    public void testGetDifferenceForCreate() {
        referenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        referenceType.setCode("RT");
        referenceType.setDescription("RTD");
        referenceType.setReferenceDocumentCategoryCode("BILLDOC");
        when(mapChange.getEntryChanges()).thenReturn(entryChanges);
        when(diff.getChanges()).thenReturn(changes);
        when(mapChange.getEntryChanges()).thenReturn(entryChanges);
        when(mapChange.getEntryValueChanges()).thenReturn(entryValuesChanges);
        when(mapChange.getEntryAddedChanges()).thenReturn(entryAddChanges);
        when(mapChange.getEntryRemovedChanges()).thenReturn(entryRemovedChanges);
        List<ChangedAttribute> diffList = auditLogHelper.getChangedAttributeDifference(referenceType);
        assertEquals(3, diffList.size());
        assertEquals("RTD", diffList.get(0).getNewValue());
    }

    @Test
    public void testGetDifferenceForUpdate() {
        newReferenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        newReferenceType.setCode("NRT");
        oldReferenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        oldReferenceType.setCode("ORT");
        Mockito.when(auditOldDataContext.get()).thenReturn(oldReferenceType);
        when(diff.getChanges()).thenReturn(changes);
        when(mapChange.getEntryChanges()).thenReturn(entryChanges);
        when(mapChange.getEntryValueChanges()).thenReturn(entryValuesChanges);
        when(mapChange.getEntryAddedChanges()).thenReturn(entryAddChanges);
        when(mapChange.getEntryRemovedChanges()).thenReturn(entryRemovedChanges);
        List<ChangedAttribute> diffList = auditLogHelper.getChangedAttributeDifference(newReferenceType);
        assertEquals(1, diffList.size());
        assertEquals("ORT", diffList.get(0).getOldValue());
        assertEquals("NRT", diffList.get(0).getNewValue());
    }

    @Test
    public void testGetDifferenceForUpdate01() {
        when(diff.getChanges()).thenReturn(changes);
        when(mapChange.getEntryChanges()).thenReturn(entryChanges);
        when(mapChange.getEntryValueChanges()).thenReturn(entryValuesChanges);
        when(mapChange.getEntryAddedChanges()).thenReturn(entryAddChanges);
        when(mapChange.getEntryRemovedChanges()).thenReturn(entryRemovedChanges);
        auditLogHelper.getChangedAttributeDifference(null);
    }

    @Test
    public void testGetDifferenceForAdd() {
        newReferenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        newReferenceType.setCode("NRT");
        oldReferenceType = null;
        when(diff.getChanges()).thenReturn(changes);
        when(mapChange.getEntryChanges()).thenReturn(entryChanges);
        when(mapChange.getEntryValueChanges()).thenReturn(entryValuesChanges);
        when(mapChange.getEntryAddedChanges()).thenReturn(entryAddChanges);
        List<ChangedAttribute> diffList = auditLogHelper.getChangedAttributeDifference(newReferenceType);
        assertEquals(1, diffList.size());
        assertEquals("NRT", diffList.get(0).getNewValue());
    }

    @Test
    public void testGetDifferenceForRemove() {
        newReferenceType = null;
        oldReferenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        oldReferenceType.setCode("ORT");
        Mockito.when(auditOldDataContext.get()).thenReturn(oldReferenceType);
        when(diff.getChanges()).thenReturn(changes);
        when(mapChange.getEntryChanges()).thenReturn(entryChanges);
        when(mapChange.getEntryValueChanges()).thenReturn(entryValuesChanges);
        when(mapChange.getEntryRemovedChanges()).thenReturn(entryRemovedChanges);
        List<ChangedAttribute> diffList = auditLogHelper.getChangedAttributeDifference(null);
        assertEquals(1, diffList.size());
        assertEquals("ORT", diffList.get(0).getOldValue());
    }

    @Test
    public void testUpdateConfigChange() {
        newReferenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        newReferenceType.setCode("NRT");
        oldReferenceType = Struct.create(cds.gen.configurationservice.ReferenceTypes.class);
        oldReferenceType.setCode("ORT");
        auditLogHelper.logConfigChange(cds.gen.configurationservice.ReferenceTypes_.CDS_NAME.toString(), Action.UPDATE, newReferenceType);

    }


}