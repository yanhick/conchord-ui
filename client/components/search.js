import React from 'react';

export default React.createClass({

    getInitialState: function () {
        return {
            value: ''
        };
    },

    handleSubmit: function (e) {
        e.preventDefault();

        const searchText = this.refs.search.getDOMNode().value;
        this.props.onSearch(searchText);
    },

    handleChange: function (e) {
        this.setState({value: e.target.value});
    },

    render: function () {
        const value = this.state.value;
        return (
            <form onSubmit={this.handleSubmit}>
                <input ref="search"
                       value={value}
                       type="search"
                       onChange={this.handleChange} />
                <input type="submit" />
            </form>
        );
    }

});
