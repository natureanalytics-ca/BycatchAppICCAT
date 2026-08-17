//UI Shield
$(document).ready(function() {
        // Instant trigger when ANY file input selection changes
        $(document).on('change', 'input[type=\"file\"]', function() {
          // Confirm a file was actually chosen before showing shield
          if (this.files.length > 0) {
            $('#ui-shield').show();
          }
        });
      });
      


// Browser-side warnings
$(document).on('shiny:inputchanged', function(event) {
  
      if (event.name === 'observerdata_upload-observer_year') {
        
        // 1. Find the picker element in the DOM
        let picker = $('#observerdata_upload-observer_year');
        let warningDiv = $('#observerdata_upload-observer_yearWarning');
        
        // 2. Extract all currently available choices from the <option> tags
        let allChoices = picker.find('option').map(function() {
          return $(this).val();
        }).get();
        
        // 3. Get the user's current selections
        let selectedValues = [].concat(event.value || []);
        
        // 4. Check conditions entirely in the browser
        let containsYear = allChoices.includes('Year');
        let selectedYear = selectedValues.includes('Year');
        let selectedSomething = selectedValues.length > 0;
        
        // Trigger if Year is an option, something is selected, but Year is not part of it
        if (containsYear && selectedSomething && !selectedYear) {
          warningDiv.show(); // Makes the text visible
        } else {
          warningDiv.hide(); // Hides the text immediately when condition is no longer met
        }
      }
      
      if (event.name === 'observerdata_upload-observer_year' || event.name === 'observerdata_upload-observer_catch') {
        
        // Fetch the values for both pickers from the DOM
        // Shiny stores multiple selections as standard JS arrays or null if empty
        let teamA = $('#observerdata_upload-observer_year').val() || [];
        let teamB = $('#observerdata_upload-observer_catch').val() || [];
        let warningDiv = $('#observerdata_upload-observer_catchWarning');
        
        // Compare the arrays to look for overlaps/duplicates
        // .some() checks if at least one item in teamA exists in teamB
        let hasOverlap = teamA.some(member => teamB.includes(member));
        
        // 4. Update the UI text display immediately
        if (hasOverlap) {
          warningDiv.show();
        } else {
          warningDiv.hide();
        }
      }
      
      if (event.name === 'logbookdata_upload-logbook_year') {
        
        // 1. Find the picker element in the DOM
        let picker = $('#logbookdata_upload-logbook_year');
        let warningDiv = $('#logbookdata_upload-logbook_yearWarning');
        
        // 2. Extract all currently available choices from the <option> tags
        let allChoices = picker.find('option').map(function() {
          return $(this).val();
        }).get();
        
        // 3. Get the user's current selections
        let selectedValues = [].concat(event.value || []);
        
        // 4. Check conditions entirely in the browser
        let containsYear = allChoices.includes('Year');
        let selectedYear = selectedValues.includes('Year');
        let selectedSomething = selectedValues.length > 0;
        
        // Trigger if Year is an option, something is selected, but Year is not part of it
        if (containsYear && selectedSomething && !selectedYear) {
          warningDiv.show(); // Makes the text visible
        } else {
          warningDiv.hide(); // Hides the text immediately when condition is no longer met
        }
      }
      
      
      if (event.name === 'datachecks_setup-logbook_effort' || event.name === 'datachecks_setup-logbook_aggregationcolumn') {
        
        // Fetch the values for both pickers from the DOM
        // Shiny stores multiple selections as standard JS arrays or null if empty
        let teamA = $('#datachecks_setup-logbook_effort').val() || [];
        let teamB = $('#datachecks_setup-logbook_aggregationcolumn').val() || [];
        let warningDiv = $('#datachecks_setup-logbook_aggregationcolumnWarning');
        
        console.log(teamA)
        console.log(teamB)
        // Compare the arrays to look for overlaps/duplicates
        // .some() checks if at least one item in teamA exists in teamB
        let hasOverlap = teamA.some(member => teamB.includes(member));
        
        // 4. Update the UI text display immediately
        if (hasOverlap) {
          warningDiv.show();
        } else {
          warningDiv.hide();
        }
      }
    });