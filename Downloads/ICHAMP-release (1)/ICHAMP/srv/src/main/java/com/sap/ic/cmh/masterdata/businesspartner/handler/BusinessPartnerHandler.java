package com.sap.ic.cmh.masterdata.businesspartner.handler;

import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.annotation.RequestScope;

import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.Before;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.address.service.AddressService;
import com.sap.ic.cmh.masterdata.businesspartner.service.BusinessPartnerService;
import com.sap.ic.cmh.masterdata.businesspartner.validations.BusinessPartnerValidator;
import com.sap.ic.cmh.masterdata.companycode.service.CompanyCodeService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.masterdataservice.Addresses;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.BusinessPartners_;
import cds.gen.masterdataservice.CompanyCodes;

@Component
@RequestScope
@ServiceName("MasterDataService")
public class BusinessPartnerHandler implements EventHandler {

	@Autowired
	private AddressService addressService;
	@Autowired
	CompanyCodeService companyCodeService;

	@Autowired
	private Messages messages;

	@Autowired
	private BusinessPartnerValidator businessPartnerValidator;
	@Autowired
	BusinessPartnerService businessPartnerService;
	@Autowired
	@Qualifier("MasterDataService")
	private CdsService cdsService;

	public static final Logger logger = LoggerHelper.getLogger(BusinessPartnerHandler.class);

	@Before(event = CdsService.EVENT_CREATE, entity = BusinessPartners_.CDS_NAME)
	public void beforeBusinessPartnersCreate(CdsCreateEventContext context, BusinessPartners businessPartnerItem){
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeBusinessPartnersCreate");
		businessPartnerValidator.checkInputsSanitized(businessPartnerItem);
		validateBusinessPartnerType(businessPartnerItem);
		BusinessPartners businessPartners = businessPartnerService
		.getBusinessPartnersBasedOnNumber(businessPartnerItem.getBusinessPartnerNumber());
		validateCustomerAndVendor(businessPartnerItem, businessPartners);
		setAddressId(businessPartnerItem);
		setCompanyCodeId(businessPartnerItem);

		messages.throwIfError();

		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeBusinessPartnersCreate");
	}


	/**
	 * This method is used to Create BusinessPartner Records
	 *
	 * @param context cdsCreateContext
	 */
	@On(event = CdsService.EVENT_CREATE, entity = BusinessPartners_.CDS_NAME)
	public void onBusinessPartnersCreate(CdsCreateEventContext context, BusinessPartners businessPartnerItem) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "onBusinessPartnersCreate");
		BusinessPartners businessPartners = businessPartnerService
		.getBusinessPartnersBasedOnNumber(businessPartnerItem.getBusinessPartnerNumber());
		
		if (null!=businessPartners) {
			String businessPartnerId =businessPartners.getId();
			businessPartnerItem.setId(businessPartnerId);
			CqnUpdate update = Update.entity(BusinessPartners_.class).data(businessPartnerItem);
			context.setResult(cdsService.run(update));
			context.setCompleted();
		}

		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "onBusinessPartnersCreate");
	}

	@Before(event = CdsService.EVENT_UPDATE, entity = BusinessPartners_.CDS_NAME)
	public void beforeBusinessPartnersUpdate(CdsUpdateEventContext context, BusinessPartners businessPartnerItem) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "beforeBusinessPartnersUpdate");
		logger.info("Inside Business Partner Update");
		businessPartnerValidator.checkInputsSanitized(businessPartnerItem);
		validateBusinessPartnerType(businessPartnerItem);
		BusinessPartners businessPartners = businessPartnerService
		.getBusinessPartnersBasedOnNumber(businessPartnerItem.getBusinessPartnerNumber());
		validateCustomerAndVendor(businessPartnerItem, businessPartners);

		setAddressId(businessPartnerItem);
		setCompanyCodeId(businessPartnerItem);
		messages.throwIfError();
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "beforeBusinessPartnersUpdate");
	}
	
	/**
	 * Get Address ID based on address and set to Business Partner entity
	 * 
	 * @param businessPartnerItem
	 */
	public void setAddressId(BusinessPartners businessPartnerItem) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "setAddressId");
		Addresses address = addressService.fetchAddress(businessPartnerItem.getAddress(),
				MessageKeys.ADDRESS_DOES_NOT_EXIST, BusinessPartners_.class, BusinessPartners_::address);
		if (address != null) {
			businessPartnerItem.setAddressIDId(address.getId());
		}
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "setAddressId");
	}

	/**
	 * Get Company Code ID based on company code and set to Business Partner entity
	 * 
	 * @param businessPartnerItem
	 */
	public void setCompanyCodeId(BusinessPartners businessPartnerItem) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "setCompanyCodeId");
		CompanyCodes companyCode = companyCodeService.fetchCompanyCode(businessPartnerItem.getCompanyCode(),
				MessageKeys.COMPANY_CODE_DOES_NOT_EXIST, BusinessPartners_.class, BusinessPartners_::companyCode);
		if (companyCode != null) {
			businessPartnerItem.setCompanyCodeIDId(companyCode.getId());
		}
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "setCompanyCodeId");
	}

	/**
	 * Validate Business Partner Type
	 * 
	 * @param businessPartnerItem
	 */
	public void validateBusinessPartnerType(BusinessPartners businessPartnerItem) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateBusinessPartnerType");
		if (businessPartnerItem.getBusinessPartnerType() != null
				&& (!businessPartnerItem.getBusinessPartnerType().isEmpty())) {
			if (!((businessPartnerItem.getBusinessPartnerType()
					.equals(Constants.EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE))
					|| (businessPartnerItem.getBusinessPartnerType()
							.equals(Constants.EXCHANGE_PARTNER_TYPE_SUPPLER_CONTACT))
					|| (businessPartnerItem.getBusinessPartnerType()
							.equals(Constants.EXCHANGE_PARTNER_TYPE_SUPPLER)))) {

				messages.error(MessageKeys.BUSINESS_PARTNER_TYPE_VALIDATION_ERROR).target("in", BusinessPartners_.class,
						BusinessPartners_::businessPartnerType);

			}
		} else {
			messages.error(MessageKeys.BUSINESS_PARTNER_TYPE_MANDATORY_ERROR).target("in", BusinessPartners_.class,
					BusinessPartners_::businessPartnerType);
		}
		LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "validateBusinessPartnerType");
	}

	/**
	 * Validate Vendor and Customer Codes
	 * 
	 * @param businessPartnerItem
	 * @param businessPartnerOptional
	 */
	public void validateCustomerAndVendor(BusinessPartners businessPartnerItem,
			BusinessPartners businessPartnerDetails) {
		LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateCustomerAndVendor");
		BusinessPartners checkIfCustomerCodeExists =null;
		BusinessPartners checkIfVendorCodeExists = null;
		if (!ObjectUtils.isEmpty(businessPartnerItem.getCustomerCode())) {
			 checkIfCustomerCodeExists = businessPartnerService.checkIfCustomerCodeExists(businessPartnerItem.getCustomerCode());
		}

		if (!ObjectUtils.isEmpty(businessPartnerItem.getVendorCode())) {
			 checkIfVendorCodeExists = businessPartnerService.checkIfVendorCodeExists(businessPartnerItem.getVendorCode());
		}

		if (((null!=checkIfCustomerCodeExists) && null==businessPartnerDetails)) {
			messages.error(MessageKeys.CUSTOMER_IS_ALREADY_AVAILABLE).target("in", BusinessPartners_.class,
					BusinessPartners_::customerCode);
		}
		if (((null!=checkIfVendorCodeExists) && null==businessPartnerDetails)) {
			messages.error(MessageKeys.VENDOR_IS_ALREADY_AVAILABLE).target("in", BusinessPartners_.class,
					BusinessPartners_::vendorCode);
		}
	  LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "validateCustomerAndVendor");
	}
}
