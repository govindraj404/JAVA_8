using CostCollectorService from '../services/costCollectorService';

annotate CostCollectorService.Complaints with @(restrict : [
    {
        grant : ['CREATE', 'AddFR', 'AddSUBL'],
        to    : 'CostCollector.Create'
    },
    {
        grant : ['READ'],
        to    : 'CostCollector.Read'
    },
    {
        grant : ['UPDATE'],
        to    : 'CostCollector.Update'
    }
]);

annotate CostCollectorService.CostCollectors with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'CostCollector.Create'
    },
    {
        grant : ['READ'],
        to    : 'CostCollector.Read'
    },
    {
        grant : ['UPDATE', 'EditFR', 'EditSUBL'],
        to    : 'CostCollector.Update'
    },
    {
        grant : ['Delete'],
        to    : 'CostCollector.Delete'
    }
]);