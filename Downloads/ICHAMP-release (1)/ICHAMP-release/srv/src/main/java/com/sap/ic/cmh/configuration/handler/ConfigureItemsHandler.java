package com.sap.ic.cmh.configuration.handler;

import static com.sap.cds.ResultBuilder.selectedRows;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.sap.cds.Result;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.ic.cmh.configuration.service.ConfigureItemService;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.ConfigureItems;
import cds.gen.configurationservice.ConfigureItems_;

@Component
@ServiceName("ConfigurationService")
public class ConfigureItemsHandler implements EventHandler {

	@Autowired
	ConfigureItemService configureItemService;

	private static final Logger logger = LoggerFactory.getLogger(ConfigureItemsHandler.class);
	private static final String CONFIG_ITEMS_HANDLER = "ConfigureItemsHandler";
	private static final String BEFORE_CONFIGITEMS_READ = "beforeConfigureItemsRead";

	/**
	 * This method is used to get all configure items and filter/search the code
	 * 
	 * @param context
	 * @return
	 */
	@On(event = { CdsService.EVENT_READ }, entity = ConfigureItems_.CDS_NAME)
	public void beforeConfigureItemsRead(CdsReadEventContext context) {
		LoggerHelper.logMethodEntry(logger, CONFIG_ITEMS_HANDLER, BEFORE_CONFIGITEMS_READ);
		ParameterInfo parameterInfo = context.getParameterInfo();
		Map<String, String> map1 = parameterInfo.getQueryParams();
		String filter = map1.get("$filter");
		String search = map1.get("$search");
		logger.info("***Filter*** {}  ", filter);
		logger.info("***search*** {}  ", search);
		List<ConfigureItems> configureItemsFilteredList = new LinkedList<>();
		List<ConfigureItems> configureItems = configureItemService.getAllConfiguredApplications();
		if (!CollectionUtils.isEmpty(configureItems)) {
			if (null != filter) {
				logger.info("***inside before filter*** ");
				getFilteredConfigureItems(filter,configureItemsFilteredList,configureItems,search);
			} else if (null != search) {
				logger.info("***inside before search*** ");
				getSearchedConfigureItems(search, configureItemsFilteredList, configureItems);
			} else {
				logger.info("**Final list without any filter and search  {} ", configureItemsFilteredList);
				configureItemsFilteredList.addAll(configureItems);
			}
		}
		List<ConfigureItems> configureItemsDistinctList = configureItemsFilteredList.stream().distinct().collect(Collectors.toList());
		Result result = selectedRows(configureItemsDistinctList).inlineCount(configureItemsDistinctList.size())
				.result();
		context.setResult(result);
		LoggerHelper.logMethodExit(logger, CONFIG_ITEMS_HANDLER, BEFORE_CONFIGITEMS_READ);
	}

	/**
	 * This method is to search a particular code and return the searched record
	 * 
	 * @param search
	 * @param configureItemsFilteredList
	 * @param configureItems
	 */
	public void getSearchedConfigureItems(String search, List<ConfigureItems> configureItemsFilteredList,
			List<ConfigureItems> configureItems) {
		LoggerHelper.logMethodEntry(logger, CONFIG_ITEMS_HANDLER, BEFORE_CONFIGITEMS_READ);
		List<ConfigureItems> list;
		String[] valuesInQuotes = StringUtils.substringsBetween(search, "\"", "\"");
		String replaceAll = valuesInQuotes[0];
		logger.info("search replaceAll :: {} ",replaceAll);
		list = configureItems.stream().filter(configureItem -> null != configureItem.getCode()
				||null != configureItem.getName()
						? null != configureItem.getCode() && configureItem.getCode().toLowerCase()
								.contains(replaceAll.toLowerCase()) ||
								null != configureItem.getCode() && configureItem.getName().toLowerCase()
								.contains(replaceAll.toLowerCase())
						: configureItem.getAssociatedApplications().stream().anyMatch(
								c -> c.getCode().toLowerCase().contains(replaceAll.toLowerCase())) ||
						configureItem.getAssociatedApplications().stream().anyMatch(
								c -> c.getName().toLowerCase().contains(replaceAll.toLowerCase()))).collect(Collectors.toList());
		logger.info("searched list  :: {} ",list);
		configureItemsFilteredList.addAll(list);
		LoggerHelper.logMethodExit(logger, CONFIG_ITEMS_HANDLER, BEFORE_CONFIGITEMS_READ);
	}

	public void getFilteredConfigureItems(String filter, List<ConfigureItems> configureItemsFilteredList,
			List<ConfigureItems> configureItems, String search) {
		List<ConfigureItems> list;
		String[] split = filter.split(" ");
		for (String s : split) {
			if (s.contains("'")) {
				String nextFilteredReplaceAll = s.contains("')") ? s.replace("')", "").replace("'", "")
						: s.replace("'", "");
				list = configureItems.stream().filter(configureItem -> null != configureItem.getCode()
						&& configureItem.getCode().toLowerCase().contains(nextFilteredReplaceAll.toLowerCase())
								? null != configureItem.getCode() && configureItem.getCode().toLowerCase()
										.contains(nextFilteredReplaceAll.toLowerCase())
								: configureItem.getAssociatedApplications().stream().anyMatch(
										c -> c.getCode().toLowerCase().contains(nextFilteredReplaceAll.toLowerCase())))
						.collect(Collectors.toList());
				configureItemsFilteredList.addAll(list);
				logger.info("configureItemsFilteredList :: {} ",configureItemsFilteredList);
			}
		}
		if(null!=search) {
			logger.info("***search after filter*** ");
			getSearchedConfigureItems(search, configureItemsFilteredList, configureItems);
		}
	}
}
