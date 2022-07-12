package com.sap.ic.cmh.claim.model;


import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;


public class ClaimItem {

    @JsonProperty("material")
    private String material;
    @JsonProperty("handle")
    private String handle;
    @JsonProperty("handleVersion")
    private String handleVersion;
    @JsonProperty("itemType")
    private String itemType;
    @JsonProperty("quantity")
    private String quantity;
    @JsonProperty("unit")
    private String unit;
    @JsonProperty("subItemType")
    private String subItemType;
    @JsonProperty("description")
    private String description;
    @JsonProperty("itemKey")
    private String itemKey;
    @JsonProperty("partCauseDamage")
    private String partCauseDamage;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<>();

    @JsonProperty("material")
    public String getMaterial() {
        return material;
    }

    @JsonProperty("material")
    public void setMaterial(String material) {
        this.material = material;
    }

    @JsonProperty("handle")
    public String getHandle() {
        return handle;
    }

    @JsonProperty("handle")
    public void setHandle(String handle) {
        this.handle = handle;
    }

    @JsonProperty("handleVersion")
    public String getHandleVersion() {
        return handleVersion;
    }

    @JsonProperty("handleVersion")
    public void setHandleVersion(String handleVersion) {
        this.handleVersion = handleVersion;
    }

    @JsonProperty("itemType")
    public String getItemType() {
        return itemType;
    }

    @JsonProperty("itemType")
    public void setItemType(String itemType) {
        this.itemType = itemType;
    }

    @JsonProperty("quantity")
    public String getQuantity() {
        return quantity;
    }

    @JsonProperty("quantity")
    public void setQuantity(String quantity) {
        this.quantity = quantity;
    }

    @JsonProperty("unit")
    public String getUnit() {
        return unit;
    }

    @JsonProperty("unit")
    public void setUnit(String unit) {
        this.unit = unit;
    }

    @JsonProperty("subItemType")
    public String getSubItemType() {
        return subItemType;
    }

    @JsonProperty("subItemType")
    public void setSubItemType(String subItemType) {
        this.subItemType = subItemType;
    }

    @JsonProperty("description")
    public String getDescription() {
        return description;
    }

    @JsonProperty("description")
    public void setDescription(String description) {
        this.description = description;
    }

    @JsonProperty("itemKey")
    public String getItemKey() {
        return itemKey;
    }

    @JsonProperty("itemKey")
    public void setItemKey(String itemKey) {
        this.itemKey = itemKey;
    }

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

     public String getPartCauseDamage() {
		return partCauseDamage;
	}

	public void setPartCauseDamage(String partCauseDamage) {
		this.partCauseDamage = partCauseDamage;
	}

}
