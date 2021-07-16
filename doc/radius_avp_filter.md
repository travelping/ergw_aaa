# RADIUS AVP filter

With the introduction of support of RADIUS vendor dictionaries in `ergw_aaa`, the possibility to filter out Attributes in the RADIUS messages sent was also added.

## Functionality

The filter can remove all Attributes for vendor ID or specific ones with combination of Vendor ID and Attribute ID as well as removing non vendor specific Attributes by ID.

The definition of the filter is provided with numeric Vendor or Attribute IDs 

e.g. this filter definition includes all 3 possible formats :
    `[100, {vendor, 52315}, {10415, 20}]`

The entries :

`100`             - removes the not vendor specific `Framed_IPv6_Pool` attribute which has Attribute ID 100

`{vendor, 52315}` - removes all the `Ituma` (vendor ID 52315) specific Attributes

`{10415, 20}`     - removes the 3GPP (vendor ID 10415) specific `User Location` Attribute with ID 20

## Configuration

The filter can be configured on `ergw_aaa`, `handler` in the `ergw_aaa_radius` section by adding the `avp_filter` configuration to either of these sections.

e.g. 
```
[
    ...
    {ergw_aaa, [
        ...
        {handlers, [
            ...
            {ergw_aaa_radius, [
                ...
                {avp_filter, [
                    100,
                    {vendor, 52315},
                    {10415, 20}
                ]}
                ...
            ]}
            ...
        ]}
        ...
    ]}
    ...
]