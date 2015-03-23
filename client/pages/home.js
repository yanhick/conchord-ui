import React from 'react';

import Search from '../components/search';

export default React.createClass({

    /* TODO: This is needed to make 'this.context.router'
     * appear, don't know why, will have to understand
     */
    contextTypes: {
        router: React.PropTypes.object.isRequired
    },

    handleSearch: function (query) {
        this.context.router.transitionTo('search', {query: query});
    },

    render: function () {
        return (
            <Search onSearch={this.handleSearch} />
        );
    }

});
