package com.sap.ic.cmh.costcollector.service;


import cds.gen.complaintservice.Complaints;
import cds.gen.costcollectorservice.CostCollectors;
import com.sap.ic.cmh.costcollector.model.ExchangeRateRequest;
import com.sap.ic.cmh.costcollector.persistance.CostCollectorDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.network.service.HttpService;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.configuration.persistency.DestinationConfigurationDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialUnitDao;
import com.sap.ic.cmh.configuration.persistency.ServiceMaterialDao;
import org.slf4j.Logger;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import cds.gen.configurationservice.ServiceMaterials;
import cds.gen.configurationservice.ServiceMaterialUnits;
import com.sap.cds.Result;
import com.sap.cds.services.messages.Messages;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;


@Service
public class CostCollectorServiceImpl implements CostCollectorService {
    @Autowired
    CostCollectorDao costCollectorDao;

    @Autowired
    HttpService httpService;

    @Autowired
    CommonFunctions commonFunctions;

    @Autowired
    Messages messages;

    @Autowired
    DestinationConfigurationDao destinationConfigDao;

    @Autowired
    ServiceMaterialDao serviceMaterialDao;

    @Autowired
    ServiceMaterialUnitDao serviceMaterialUnitDao;

    public static final Logger logger = LoggerHelper.getLogger(CostCollectorServiceImpl.class);
    private static final String COST_COLLECTOR_SERVICE_IMPL = "CostCollectorServiceImpl";

    /**
     * Insert the costCOllector
     * 
     * @param costCollector
     * @return CostCollectors
     */
    @Override
    public CostCollectors insertCostCollector(CostCollectors costCollector) {
        return costCollectorDao.insertCostCollector(costCollector);
    }


    /**
     * Update the costCOllector
     * 
     * @param costCollector
     * @return CostCollectors
     */
    @Override
    public CostCollectors updateCostCollector(CostCollectors costCollector) {
        return costCollectorDao.updateCostCollector(costCollector);
    }


    /**
     * Delete the costCOllector
     * 
     * @param costCollector
     * @return CostCollectors
     */
    @Override
    public CostCollectors deleteCostCollector(CostCollectors costCollector) {
        return costCollectorDao.deleteCostCollector(costCollector);
    }


    /**
     * Select the costCOllector which has to be transferred to claim
     *
     * @param complaintId
     * @return List<CostCollectors>
     */
    @Override
    public List<CostCollectors> selectTransferToClaimCostCollector(String complaintId) {
        LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_SERVICE_IMPL, "selectTransferToClaimCostCollector");
        Result selectTransferToClaimCostCollectorResult = costCollectorDao.selectTransferToClaimCostCollector(complaintId);
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_SERVICE_IMPL, "selectTransferToClaimCostCollector");
        return selectTransferToClaimCostCollectorResult.first().isPresent() ? selectTransferToClaimCostCollectorResult.listOf(CostCollectors.class) : null;
    }

    /**
     * Select All the costCOllector of a complaint
     *
     * @param complaintId
     * @return List<CostCollectors>
     */
    @Override
    public List<CostCollectors> selectAllCostCollector(String complaintId) {
        LoggerHelper.logMethodEntry(logger, COST_COLLECTOR_SERVICE_IMPL, "selectAllCostCollector");
        Result selectAllCostCollectorResult = costCollectorDao.selectAllCostCollector(complaintId);
        LoggerHelper.logMethodExit(logger, COST_COLLECTOR_SERVICE_IMPL, "selectAllCostCollector");
        return selectAllCostCollectorResult.first().isPresent() ? selectAllCostCollectorResult.listOf(CostCollectors.class) : null;
    }

    /**
     * Fetch exchange rate and convert the cost
     *
     * @param costCollector
     * @param complaint
     * @return BigDecimal
     */
    @Override
    public BigDecimal calculateExchangeRateAndConvertCost(CostCollectors costCollector, Complaints complaint) {
        String exchangeRate = "1";
        ResponseModel responseFromCpi=null;
        if(!costCollector.getCurrencyCode().equals(complaint.getCurrencyCode())) {
            Map<String,Object> exchRateRequestMap = createExchangeRateDTO("M", costCollector.getCurrencyCode(), complaint.getCurrencyCode());
            try {
            	responseFromCpi = httpService.callCpiFlow(Constants.EXCHANGE_RATE_APPENDED_URL, exchRateRequestMap, fetchConfiguredDestination(complaint.getCompanyCodeId(),Constants.CLAIM_CODE));
                exchangeRate=null!=responseFromCpi ? responseFromCpi.getResult() : null;
            } catch (IOException e) {
                messages.error(MessageKeys.ERROR_IN_CPI_FLOW + ":" +e.toString());
            }
        }
        if(null!=exchangeRate) {
            BigDecimal rate = new BigDecimal(exchangeRate);
            BigDecimal tcost = new BigDecimal(costCollector.getTotalCost().toString());
            return tcost.multiply(rate);
        }
        return null;
    }

    @Override
    public Map<String,Object> createExchangeRateDTO(String rateType, String fromCurrency, String toCurrency){
        ExchangeRateRequest exchRateRequest = new ExchangeRateRequest();
        exchRateRequest.setRateType(rateType);
        exchRateRequest.setFromCurrency(fromCurrency);
        exchRateRequest.setToCurrency(toCurrency);
        return commonFunctions.convertObjectToMap(exchRateRequest);
    }

    /**
     * Fetch configured destination for Cost Collector
     * @param companyID
     * @param claimCode
     * @return
     */
    @Override
	public String fetchConfiguredDestination(String companyID, String claimCode) {
        String destination = "";
		Result destinationConfigs =
		        destinationConfigDao.getDestinationConfigBasedOnCompanyAndBOType(companyID,claimCode);
		if (destinationConfigs!=null && destinationConfigs.first().isPresent()) {
			 destination = destinationConfigs.list().get(0).get(Constants.DESTINATION).toString();
		}
		return destination;
	}


    @Override
    public CostCollectors getCostCollector(String id) {
        return costCollectorDao.getCostCollector(id).single(CostCollectors.class);
    }

    @Override
    public BigDecimal convertQuantityOfFR(CostCollectors costCollector){
        
        BigDecimal quantity = new BigDecimal(costCollector.getQuantity().toString());
        if(!costCollector.getUnitCode().equals(Constants.DEFAULT_LABOR_UNIT)){
            //get material based on sub-item type
            Result serviceMaterialResult = serviceMaterialDao.getServiceMaterialsBasedOnSubItemType(costCollector.getSubItemTypeCode());
            if(serviceMaterialResult!=null && serviceMaterialResult.first().isPresent()){
                List<ServiceMaterials> serviceMaterialList = serviceMaterialResult.listOf(ServiceMaterials.class);
                Result serviceMaterialUnitResult = serviceMaterialUnitDao.getServiceMaterialUnitBasedOnMaterialIdAndUnit(serviceMaterialList.get(0).getId(), costCollector.getUnitCode());
                if(serviceMaterialUnitResult.first().isPresent() && costCollector.getQuantity()!=null && StringUtils.isNotBlank(costCollector.getUnitCode())){
                    ServiceMaterialUnits serviceMaterialUnit= serviceMaterialUnitResult.single(ServiceMaterialUnits.class);
                    quantity = (quantity.multiply(serviceMaterialUnit.getNumerator())).divide(serviceMaterialUnit.getDenominator(),2,RoundingMode.HALF_UP);
                }
                else{
                    quantity = null;
                    messages.error(MessageKeys.UNIT_NOT_CONFIGURED_IN_SERVICE_MATERIAL_UNIT);
                }
            }
        }
        return quantity;
    }
}