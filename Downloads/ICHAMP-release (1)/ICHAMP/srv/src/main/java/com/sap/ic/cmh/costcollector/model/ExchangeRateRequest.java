package com.sap.ic.cmh.costcollector.model;


public class ExchangeRateRequest {
    
    private String rateType;
    private String fromCurrency;
    private String toCurrency;

    /**
     * @return the rateType
     */
    public String getRateType() {
        return rateType;
    }
    /**
     * @param rateType the rateType to set
     */
    public void setRateType(String rateType) {
        this.rateType = rateType;
    }
    /**
     * @return the fromCurrency
     */
    public String getFromCurrency() {
        return fromCurrency;
    }
    /**
     * @param fromCurrency the fromCurrency to set
     */
    public void setFromCurrency(String fromCurrency) {
        this.fromCurrency = fromCurrency;
    }
    /**
     * @return the toCurrency
     */
    public String getToCurrency() {
        return toCurrency;
    }
    /**
     * @param toCurrency the toCurrency to set
     */
    public void setToCurrency(String toCurrency) {
        this.toCurrency = toCurrency;
    }    
}
