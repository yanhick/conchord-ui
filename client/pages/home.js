import React from 'react';

import Search from '../components/search';
import Actions from '../actions/';

export default React.createClass({

    handleSearch: function (query) {
        Actions.searchSong(query);
    },

    render: function () {
        return (
            <Search onSearch={this.handleSearch} />
        );
    }

});

