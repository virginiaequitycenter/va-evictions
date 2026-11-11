// Draft: info-icon tooltips on the catalog table's column headers.
// Moved out of app.R on 2026-11-11, where it sat commented out inside
// datatable(callback = JS(...)). Not deployed (deploy.R doesn't upload notes/).
//
// Original to-do note: "NEED TO FIX BELOW TO CALL TO A UNIQUE TABLE ID/DATA
// TABLE ID CHANGES WITH EACH REACTIVE INPUT". Hint: inside a DT callback, the
// `table` variable is the DataTables API for the current table, so
// $(table.table().header()).find('th') avoids relying on an element ID.
// The tip text would also need Spanish versions.

var tips = ['The general district court where the case was filed. Court jurisdictions are tied to localities (counties or cities) in Virginia.',
  'The entity filing an eviction case against a tenant with the court. In Virginia, eviction cases can be filed by "the landlord, [their] agent, attorney, or other person."',
  'The total number of eviction cases filed by the plaintiff in the selected time period and jurisdiction.',
  'The total number of cases filed by the plaintiff that ended in an eviction judgment (a judgment for the plaintiff).',
  'We consider serial cases to be repeated cases filed by a given plaintiff against a given defendant in a given ZIP code within a 12-month period.',
  'Time period(s) of total filings and evictions. Shown as all years, by year, or by month based on selection above.',
  'The ZIP codes provided for the defendants (tenants) against whom the unlawful detainer/eviction is filed.'
];
var header = $('#plaintiff_table th');
for (var i = 0; i < tips.length; i++) {
  $(header[i]).append('<svg version="1.1" id="tooltip-icon" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 16 16" width="16px" height="16px" style="enable-background:new 0 0 16 16;margin-left:10px;margin-bottom:3px;" xml:space="preserve"><path d="M14.5,8c0-3.59-2.91-6.5-6.5-6.5S1.5,4.41,1.5,8s2.91,6.5,6.5,6.5S14.5,11.59,14.5,8z M0,8c0-4.42,3.58-8,8-8 s8,3.58,8,8s-3.58,8-8,8S0,12.42,0,8z M5.31,5.17C5.55,4.47,6.22,4,6.96,4h1.82c1.09,0,1.97,0.88,1.97,1.97 c0,0.71-0.38,1.36-0.99,1.71L8.75,8.26C8.74,8.67,8.41,9,8,9C7.58,9,7.25,8.67,7.25,8.25V7.83c0-0.27,0.14-0.52,0.38-0.65l1.38-0.79 C9.16,6.3,9.25,6.14,9.25,5.97c0-0.26-0.21-0.47-0.47-0.47H6.96c-0.11,0-0.2,0.07-0.23,0.17L6.71,5.71C6.57,6.1,6.14,6.3,5.75,6.16 S5.16,5.59,5.3,5.21L5.31,5.17L5.31,5.17z M7,11c0-0.55,0.45-1,1-1s1,0.45,1,1s-0.45,1-1,1S7,11.55,7,11z"/></svg>');
  $(header[i]).find('svg').attr('data-toggle', 'tooltip');
  $(header[i]).find('svg').attr('data-placement', 'top');
  $(header[i]).find('svg').attr('title', tips[i]);
}
