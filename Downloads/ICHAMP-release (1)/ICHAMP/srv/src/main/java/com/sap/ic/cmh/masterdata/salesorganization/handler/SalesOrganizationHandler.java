package com.sap.ic.cmh.masterdata.salesorganization.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.Struct;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.masterdata.salesorganization.service.SalesOrganizationService;
import com.sap.ic.cmh.masterdata.salesorganization.validation.SalesOrganizationValidation;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.CompanyCodes;
import cds.gen.masterdataservice.SalesOrganizations;
import cds.gen.masterdataservice.SalesOrganizations_;

/**
 * SalesOrganizationHandler is used to handle SalesOrganization related events
 */
@Component
@RequestScope
@ServiceName ("MasterDataService")
public class SalesOrganizationHandler implements EventHandler {

    public static final Logger logger = LoggerFactory.getLogger(SalesOrganizationHandler.class);
    @Autowired
    SalesOrganizationService salesOrganizationService;
    @Autowired
    private CompanyCodeService companyCodeService;
    @Autowired
    BusinessPartnerService businessPartnerService;
    @Autowired
    private Messages messages;
    @Autowired
    private SalesOrganizationValidation salesOrganizationValidator;
    @Autowired
    @Qualifier("MasterDataService")
	private CdsService cdsService;

    /**
     * Method to do sanitization before processing SalesOrganization for Create
     *
     * @param salesOrganizationItem
     */
    @Before (event = CdsService.EVENT_CREATE, entity = SalesOrganizations_.CDS_NAME)
    public void beforeSalesOrganizationsCreate(SalesOrganizations salesOrganizationItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeSalesOrganizationsCreate");
        messages.info(salesOrganizationItem.toJson(), salesOrganizationItem);
        salesOrganizationValidator.checkInputsSanitized(salesOrganizationItem);
        
        fetchCompanyCode(salesOrganizationItem);
        fetchBusinessPartner(salesOrganizationItem);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeSalesOrganizationsCreate");
    }


	public void fetchCompanyCode(SalesOrganizations salesOrganizationItem) {
		CompanyCodes companyCode = companyCodeService.fetchCompanyCodesBasedOnCode(salesOrganizationItem.getCompanyCode());
        if (companyCode != null) {
            salesOrganizationItem.setCompanyCodeIDId(companyCode.getId());
        }
	}


    /**
     * This method is used to create/update sales organization detail
     *
     * @param context
     * @param salesOrganizationItem
     */
    @On (event = CdsService.EVENT_CREATE, entity = SalesOrganizations_.CDS_NAME)
    public void onSalesOrganizationsCreate(CdsCreateEventContext context, SalesOrganizations salesOrganizationItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onSalesOrganizationsCreate");
        messages.info(salesOrganizationItem.toJson(), salesOrganizationItem);

        SalesOrganizations salesOrganizationDetails = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizationItem.getSalesOrganization());
        if(null!=salesOrganizationDetails) {
        	//handled patch
        	SalesOrganizations salesOrganizationsToBeUpdated = Struct.create(SalesOrganizations.class);
        	salesOrganizationsToBeUpdated.putAll(salesOrganizationItem);
        	salesOrganizationItem.putAll(salesOrganizationDetails);
        	salesOrganizationItem.putAll(salesOrganizationsToBeUpdated);
        	salesOrganizationItem.setId(salesOrganizationDetails.getId());
        	CqnUpdate update = Update.entity(SalesOrganizations_.class).data(salesOrganizationItem);
            context.setResult(cdsService.run(update));
            context.setCompleted();
        }
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onSalesOrganizationsCreate");
    }
    
    /**
     * Method to do sanitization before processing SalesOrganization for Update
     *
     * @param salesOrganizationItem
     */
    @Before (event = CdsService.EVENT_UPDATE, entity = SalesOrganizations_.CDS_NAME)
    public void beforeSalesOrganizationsUpdate(SalesOrganizations salesOrganizationItem) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeSalesOrganizationsUpdate");
        messages.info(salesOrganizationItem.toJson(), salesOrganizationItem);
        SalesOrganizations salesOrganizationDetails = salesOrganizationService.getSalesOrganizationDetailsBasedOnSalesOrgCode(salesOrganizationItem.getSalesOrganization());
        if(null!=salesOrganizationDetails) {
        	//handled patch
        	SalesOrganizations salesOrganizationsToBeUpdated = Struct.create(SalesOrganizations.class);
        	salesOrganizationsToBeUpdated.putAll(salesOrganizationItem);
        	salesOrganizationItem.putAll(salesOrganizationDetails);
        	salesOrganizationItem.putAll(salesOrganizationsToBeUpdated);
        	salesOrganizationItem.setId(salesOrganizationDetails.getId());
        }
       
        salesOrganizationValidator.checkInputsSanitized(salesOrganizationItem);

        fetchCompanyCode(salesOrganizationItem);
        fetchBusinessPartner(salesOrganizationItem);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeSalesOrganizationsUpdate");
    }


	public void fetchBusinessPartner(SalesOrganizations salesOrganizationItem) {
		BusinessPartners businessPartner = businessPartnerService.getBusinessPartnersBasedOnNumber(salesOrganizationItem.getBusinessPartner());
        if(null!=businessPartner) {
        	salesOrganizationItem.setBusinessPartnerIDId(businessPartner.getId());
        }
	}

}