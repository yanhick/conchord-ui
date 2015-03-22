import React from 'react';

import Results from '../components/results';
import Actions from '../actions/';
import Store from '../stores/';

export default React.createClass({

    handleSelect: function (id) {
        Actions.getSong(id);
    },

    render: function () {
        return (
            <Results data={Store.getSearchResults()} onSelect={this.handleSelect} />
        );
    }

});


