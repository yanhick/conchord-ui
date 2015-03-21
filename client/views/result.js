import React from 'react';

export default React.createClass({

    handleClick: function (e) {
        e.preventDefault();
        this.props.onSelect(this.props.data.href);
    },

    render: function () {
        return (
            <li>
                <a href={this.props.data.href}
                   onClick={this.handleClick}>
                    {this.props.data.title}
                </a>
            </li>
        );
    }

});
