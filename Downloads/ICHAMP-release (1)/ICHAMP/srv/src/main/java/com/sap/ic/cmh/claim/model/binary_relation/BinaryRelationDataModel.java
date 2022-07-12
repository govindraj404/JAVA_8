package com.sap.ic.cmh.claim.model.binary_relation;
import com.fasterxml.jackson.annotation.JsonProperty;

public class BinaryRelationDataModel {
    @JsonProperty("object_a")
    private BinaryRelationObject objectA;
    @JsonProperty("object_b")
    private BinaryRelationObject objectB;
    @JsonProperty("relation_type")
    private String relationType;

    public BinaryRelationObject getObjectA() {
        return objectA;
    }

    public void setObjectA(BinaryRelationObject objectA) {
        this.objectA = objectA;
    }

    public BinaryRelationObject getObjectB() {
        return objectB;
    }

    public void setObjectB(BinaryRelationObject objectB) {
        this.objectB = objectB;
    }

    public String getRelationType() {
        return relationType;
    }

    public void setRelationType(String relationType) {
        this.relationType = relationType;
    }
}
 