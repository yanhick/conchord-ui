import React from 'react';

import Store from '../stores/';
import Actions from '../actions/';

import Song from '../components/song';

function getState() {
    return {
        song: Store.getSong()
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
        Actions.getSong(this.context.router.getCurrentQuery().id);
    },

    componentWillUnmount () {
        Store.removeListener('change', this._onChange);
    },

    componentWillReceiveProps (props) {
        Actions.searchSong(this.context.router.getCurrentQuery().id);
    },

    _onChange () {
        this.setState(getState());
    },

    render () {
        return (
            <Song data={this.state.song} />
        );
    }

});

