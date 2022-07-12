using ManageReturnPurchaseOrderService from '../services/manageReturnPurchaseOrderService';

annotate ManageReturnPurchaseOrderService.ReturnPurchaseOrders with @(restrict : [
    {
        grant : ['CREATE'],
        to    : 'ReturnPurchaseOrder.Create'
    },
    {
        grant : ['READ'],
        to    : 'ReturnPurchaseOrder.Read',
        where : 'plant.plant = $user.Plant AND company.companyCode = $user.CompanyCode'
    },
    {
        grant : ['UPDATE'],
        to    : 'ReturnPurchaseOrder.Update'
    }
]);