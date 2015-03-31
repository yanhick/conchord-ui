import React from 'react';

import Search from '../components/search';
import Header from '../components/header';

export default React.createClass({

    /* TODO: This is needed to make 'this.context.router'
     * appear, don't know why, will have to understand
     */
    contextTypes: {
        router: React.PropTypes.object.isRequired
    },

    handleSearch: function (query) {
        this.context.router.transitionTo('search', {}, {q: query});
    },

    handleGoHome () {
        this.context.router.transitionTo('/');
    },

    render: function () {
        return (
            <div>
                <Header onGoHome={this.handleGoHome} />
                <Search onSearch={this.handleSearch} />
            </div>
        );
    }

});
