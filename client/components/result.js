import React from 'react';

export default React.createClass({

    handleClick: function (e) {
        e.preventDefault();
        this.props.onSelect(this.props.data.id);
    },

    render: function () {
        return (
            <li>
                <section>
                    <a href={this.props.data.href}
                       onClick={this.handleClick}>
                       <h1>{this.props.data.title}</h1>
                       <h2>{this.props.data.artist}</h2>
                       <h3>{this.props.data.album}</h3>
                    </a>
                </section>
            </li>
        );
    }

});
