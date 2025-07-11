/*
console.log("radio-button.js is loaded.")

// definice funkce
function initRadioSync() {
  console.log("initRadioSync spuštěno");

  $(document).on('change', 'input[type=\"radio\"]', function() {
    const inputName = $(this).attr('name');
    //if (inputName && inputName.includes('visual_check') && inputName.includes('-')) {
    if (inputName && inputName.includes('visual_check')) {
      console.log("radio changed:", inputName, $(this).val());

      const rowIndex = parseInt($(this).closest('.fusion-radio-group').data('row'));
      const selectedValue = $(this).val();
      const namespaceId = inputName.replace(/_\\d+$/, '') + '_changed';

      Shiny.setInputValue(namespaceId, {
        row: rowIndex,
        value: selectedValue,
        timestamp: new Date().getTime()
      });
      console.log("Radio change:", selectedValue);
    }
  });
}

// registrace na custom message handler
Shiny.addCustomMessageHandler("initRadioSync", function(message) {
  initRadioSync();
});
*/