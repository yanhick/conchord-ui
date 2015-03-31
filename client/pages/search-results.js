import React from 'react';

import Results from '../components/results';
import Header from '../components/header';

import Actions from '../actions/';
import Store from '../stores/';

function getState() {
    return {
        searchResults: Store.getSearchResults()
    };
}

export default React.createClass({

    contextTypes: {
        router: React.PropTypes.object.isRequired
    },

    getInitialState () {
        return getState();
    },

    componentDidMount () {
        Store.on('change', this._onChange);
        Actions.searchSong(this.context.router.getCurrentQuery().q);
    },

    componentWillUnmount () {
        Store.removeListener('change', this._onChange);
    },

    componentWillReceiveProps (props) {
        Actions.searchSong(this.context.router.getCurrentQuery().q);
    },

    _onChange () {
        this.setState(getState());
    },

    handleSelect (id) {
        this.context.router.transitionTo('song', {}, {id: id});
    },

    handleGoHome () {
        this.context.router.transitionTo('/');
    },

    render () {
        return (
            <div>
                <Header onGoHome={this.handleGoHome} />
                <Results data={this.state.searchResults} onSelect={this.handleSelect} />
            </div>
        );
    }

});


