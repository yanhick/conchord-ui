import React from 'react';

export default React.createClass({

    render: function () {
        return (
            <section>
                <h1>{this.props.data.title}</h1>
                <h2>{this.props.data.artist}</h2>
                <h3>{this.props.data.album}</h3>
            </section>
        );
    }

});
